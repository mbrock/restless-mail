{-# Language TemplateHaskell #-}
{-# Language DataKinds #-}
{-# Language TypeOperators #-}
{-# Language DeriveGeneric #-}
{-# Language OverloadedStrings #-}
{-# Language LambdaCase #-}

module Main where

import Control.Applicative ((<|>))
import Control.Monad (unless)

import Data.Aeson

import Data.Foldable (toList)

import Data.ByteString (ByteString)
import Data.DirStream
import Data.String
import Data.Text (Text)
import Data.Word (Word8)
import Data.List (find)
import Data.Monoid ((<>))
import Data.Ord (comparing)
import Data.Sequence (Seq)
import Data.Foldable (forM_, toList)

import Control.Lens

import qualified Data.Sequence as Seq

import Data.Thyme.Clock (UTCTime)
import Data.Thyme.Format
import Data.Thyme.Format.Aeson
import System.Locale

import Pipes hiding (Proxy)
import Pipes.Safe
import Pipes.Parse

import qualified Pipes.Attoparsec

import qualified Pipes.Prelude as Pipes
import Pipes.ByteString (fromHandle)

import System.Environment
import Filesystem
import Filesystem.Path.CurrentOS

import Data.Text.Format

import qualified Data.Attoparsec.ByteString        as A
import qualified Data.Attoparsec.ByteString.Char8  as A8
import qualified Data.ByteString as BS
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text

import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.IO as LazyText

import qualified Data.Vector as Vec

import GHC.Generics

import Servant
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors

import Brick
import Brick.Focus
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import Brick.Widgets.Edit
import Brick.Widgets.List
import qualified Graphics.Vty as Vty

data Summary = Summary
  { summaryId      :: Text
  , summaryFrom    :: Text
  , summaryDate    :: UTCTime
  , summarySubject :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON Summary

instance Ord Summary where
  compare = flip (comparing summaryDate)

data HeaderField = HeaderField
  { headerName :: ByteString
  , headerBody :: ByteString
  } deriving (Show, Ord, Eq)

type MailAPI = "summary" :> Get '[JSON] [Summary]

data UiPane = SummaryPane | BodyPane
  deriving (Eq, Ord, Show)

type UiWidget = Widget UiPane

data UiState = UiState
  { _uiSummaryList :: List UiPane Summary
  }

makeLenses ''UiState

main :: IO ()
main = do
  maildir <- fromString <$> getEnv "MAILDIR"
  summaries <- Seq.sort <$> runSafeT (scan maildir)

  _ <- defaultMain app $
    UiState
      { _uiSummaryList =
          list SummaryPane (Vec.fromList (toList summaries)) 3
      }

  return ()

app :: App UiState () UiPane
app = App
  { appDraw = drawUi
  , appChooseCursor = neverShowCursor
  , appHandleEvent = \s e ->
      case (s, e) of
        (_, VtyEvent (Vty.EvKey Vty.KEsc [])) ->
          halt s
        (_, VtyEvent e') -> do
          s' <- handleEventLensed s uiSummaryList handleListEvent e'
          continue s'
        _ ->
          continue s
  , appStartEvent = return
  , appAttrMap = const (attrMap Vty.defAttr theme)
  }

withHighlight False = withDefAttr dimAttr
withHighlight True  = withDefAttr selectedAttr

drawUi :: UiState -> [UiWidget]
drawUi ui =
  [ borderWithLabel (txt "Restless Mail") $
      renderList
        (\selected x ->
           withHighlight selected $
             vBox [ hBox
                    [ txt (summaryFrom x)
                    , padLeft Max . padRight (Pad 1) . str . show $ summaryDate x
                    ]
                  , txt (Text.take 60 (summarySubject x))
                  , txt " "
                  ])
        True
        (view uiSummaryList ui)
  ]

selectedAttr :: AttrName; selectedAttr = "selected"
dimAttr :: AttrName; dimAttr = "dim"

theme :: [(AttrName, Vty.Attr)]
theme =
  [ (selectedAttr, Vty.defAttr `Vty.withStyle` Vty.bold)
  , (dimAttr, Vty.defAttr `Vty.withStyle` Vty.dim)
  , (borderAttr, Vty.defAttr `Vty.withStyle` Vty.dim)
  ]

  -- forM_ summaries (LazyText.putStrLn . displaySummary)

  -- run 1025 $ simpleCors (serve (Proxy :: Proxy MailAPI)
  --                          (return (toList summaries)))

displaySummary :: Summary -> LazyText.Text
displaySummary x = format "{}\n{}\n{}\n{}\n"
  ( summaryId x
  , show (summaryDate x)
  , summaryFrom x
  , summarySubject x
  )

scan maildir =
  Pipes.fold (Seq.|>) mempty id .
    for (every (childOf maildir)) $ \path -> do
      result <- liftIO (mail path)
      case result of
        Just (Right x) ->
          yield x
        _ ->
          return ()

idFromPath x =
  case toText (filename x) of
    Left _ -> error "dumb"
    Right s -> fst (Text.breakOn ":" s)

mail path =
  withFile path ReadMode $ \h ->
    do x <- evalStateT
              (Pipes.Attoparsec.parse (parseSummary (idFromPath path)))
              (fromHandle h)
       return x

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

parseSummary :: Text -> A.Parser Summary
parseSummary mailId =
  do headers <- parseHeaders
     let get h =
           case find ((== h) . headerName) headers of
             Just x -> return (headerBody x)
             _      -> fail ("no header " ++ show h)
     Summary
       <$> pure mailId
       <*> fmap (Text.decodeUtf8With Text.lenientDecode) (get "from")
       <*> (get "date" >>= subparse parseDate)
       <*> fmap (Text.decodeUtf8With Text.lenientDecode) (get "subject")

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
