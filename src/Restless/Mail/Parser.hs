module Restless.Mail.Parser where

import Restless.Mail

import Control.Applicative ((<|>), many)
import Control.Lens
import Control.Monad (unless)
import Data.Aeson
import Data.ByteString (ByteString)
import Data.DirStream
import Data.Foldable (forM_, toList)
import Data.Foldable (toList)
import Data.List (find)
import Data.Monoid ((<>))
import Data.Ord (comparing)
import Data.Sequence (Seq)
import Data.String
import Data.Text (Text)
import Data.Text.Format
import Data.Thyme.Clock (UTCTime)
import Data.Thyme.Format
import Data.Thyme.Format.Aeson
import Data.Word (Word8)
import Filesystem
import Filesystem.Path.CurrentOS
import GHC.Generics
import Pipes hiding (Proxy)
import Pipes.ByteString (fromHandle)
import Pipes.Parse
import Pipes.Safe
import System.Directory (createDirectoryIfMissing, copyFile)
import System.Environment
import System.IO.Unsafe (unsafePerformIO)
import System.Locale

import qualified Codec.Binary.Base64              as Base64
import qualified Codec.Binary.QuotedPrintable     as QuotedPrintable
import qualified Data.Attoparsec.ByteString       as A
import qualified Data.Attoparsec.ByteString.Char8 as A8
import qualified Data.ByteString                  as BS
import qualified Data.Map                         as Map
import qualified Data.Sequence                    as Seq
import qualified Data.Set                         as Set
import qualified Data.Text                        as Text
import qualified Data.Text.Encoding               as Text
import qualified Data.Text.Encoding.Error         as Text (lenientDecode)
import qualified Data.Text.ICU.Convert            as ICU
import qualified Data.Text.IO                     as Text
import qualified Data.Text.Lazy                   as LazyText
import qualified Data.Text.Lazy.IO                as LazyText
import qualified Data.Vector                      as Vec
import qualified Pipes.Attoparsec
import qualified Pipes.Prelude                    as Pipes

parseTags = A.sepBy p (A8.char '\n')
  where
    p = do
      tag <- A8.takeWhile1 (/= ' ')
      from <- A8.char ' ' *> A8.takeWhile1 (/= '\n')
      pure (decodeUtf8 from, decodeUtf8 tag)

peekLineChar :: A8.Parser ()
peekLineChar =
  A.peekWord8 >>= \case
    Just x | not (A8.isEndOfLine x) ->
      return ()
    _ ->
      mzero

peekHorizontalSpace :: A8.Parser ()
peekHorizontalSpace =
  A.peekWord8 >>= \case
    Just x | A8.isHorizontalSpace x ->
      return ()
    _ ->
      mzero

getMimeWords :: ByteString -> Text
getMimeWords bs =
  case A.parseOnly mimeWords bs of
    Left s  -> error s
    Right x -> x


decoders :: Map.Map ByteString ICU.Converter
decoders = Map.fromList . map (\(x, y) -> (x, f y)) $
  [ ("utf-8", "utf-8")
  , ("iso-8859-1", "iso-8859-1")
  , ("iso-8859-15", "iso-8859-15")
  , ("cp1252", "windows-1252")
  , ("windows-1257", "windows-1257")
  , ("windows-1252", "windows-1252")
  , ("windows-1251", "windows-1251")
  ]
  where f x = unsafePerformIO (ICU.open x (Just True))

mimeWords :: A8.Parser Text
mimeWords = mconcat <$> many (p1 <|> p2)
  where
    p1 = do
      _ <- A8.char '='
      A8.anyChar >>= \case
        '?' -> do
          encoding <- A8.takeTill (== '?')
          _ <- A8.char '?'
          mode <- A8.takeTill (== '?')
          _ <- A8.char '?'
          body <- A8.takeTill (== '?')
          _ <- A8.string "?="
          let decoder =
                case Map.lookup (BS.map toLower encoding) decoders of
                  Just y -> y
                  x -> error (show x)
              decrypter =
                case BS.map toLower mode of
                  "q" -> QuotedPrintable.decode
                  "b" -> Base64.decode
          case decrypter body of
            Left _  -> error "bad email"
            Right x -> pure (ICU.toUnicode decoder x)
        x ->
          decodeUtf8 <$> A8.takeTill (== '=')
    p2 = do
      _ <- A.peekWord8'
      decodeUtf8 <$> A8.takeTill (== '=')

foldWhitespace :: A8.Parser ByteString
foldWhitespace =
  mconcat <$>
    A.sepBy
      (A.takeWhile1 (not . A8.isEndOfLine))
      (A8.endOfLine >> peekHorizontalSpace)

subparse p bs =
  case A.parseOnly p bs of
    Left s  -> fail s
    Right x -> pure x

parseSummary :: Text -> Prelude.FilePath -> A.Parser Summary
parseSummary mailId mailPath =
  do headers <- parseHeaders
     let get h =
           case find ((== h) . headerName) headers of
             Just x -> return (headerBody x)
             _      -> fail ("no header " ++ show h)
     Summary
       <$> pure mailId
       <*> pure mailPath
       <*> fmap getMimeWords (get "from")
       <*> (get "date" >>= subparse parseDate)
       <*> fmap getMimeWords (get "subject")

decodeUtf8 :: ByteString -> Text
decodeUtf8 = Text.decodeUtf8With Text.lenientDecode

parseHeaders :: A.Parser [HeaderField]
parseHeaders =
  A.sepBy parseHeaderField (A8.endOfLine >> peekLineChar)

-- ASCII-specific
toLower :: Word8 -> Word8
toLower w | w >= 65 && w <= 90 = w + 32
          | otherwise          = w

parseHeaderField :: A.Parser HeaderField
parseHeaderField =
  HeaderField
    <$> ((BS.map toLower <$> A.takeWhile1 (/= 58))
           <* A.skip (== 58)
           <* A.skip A8.isHorizontalSpace)
    <*> foldWhitespace

fromDigit :: Integral a => Word8 -> a
fromDigit w = fromIntegral (w - 48)

digits :: Integral a => Int -> A.Parser a
digits 0 = return 0
digits 1 = fromDigit <$> A.satisfy A8.isDigit_w8
digits n = do
  s <- A.take n
  unless (BS.all A8.isDigit_w8 s) $
    fail $ "expected " ++ show n ++ " digits"
  return $ BS.foldl' (\a w -> 10 * a + fromDigit w) 0 s

dayNames :: [ByteString]
dayNames =
  map (<> ", ") ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"]

parseDate :: A.Parser UTCTime
parseDate =
  do A.option () (A.choice (map A8.string dayNames) >> pure ())
     buildTime <$> timeParser defaultTimeLocale "%e %b %Y %T %Z"
