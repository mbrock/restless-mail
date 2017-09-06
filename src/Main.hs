{-# Language DataKinds #-}
{-# Language DeriveGeneric #-}
{-# Language LambdaCase #-}
{-# Language OverloadedStrings #-}
{-# Language RecordWildCards #-}
{-# Language TemplateHaskell #-}
{-# Language TypeOperators #-}

module Main where

import Restless.Mail
import Restless.Mail.Parser

import qualified Restless.Git as Git

import Control.Monad      (forM)
import Data.ByteString    (ByteString)
import Data.DirStream     (childOf)
import Data.Foldable      (toList)
import Data.Set           (Set)
import Data.Sequence      (Seq)
import Data.String        (fromString)
import Data.Text          (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Format   (format)
import Data.Thyme.Format  (formatTime)
import Filesystem         (IOMode (ReadMode), withFile)
import Pipes hiding       (Proxy)
import Pipes.ByteString   (fromHandle)
import Pipes.Parse        (evalStateT)
import Pipes.Safe         (MonadSafe, runSafeT)
import System.Environment (getEnv)
import System.Locale      (defaultTimeLocale)

import qualified Data.ByteString                  as BS
import qualified Data.Map                         as Map
import qualified Data.Set                         as Set
import qualified Data.Sequence                    as Seq
import qualified Data.Text                        as Text
import qualified Data.Text.Lazy                   as LazyText
import qualified Filesystem.Path.CurrentOS        as OS
import qualified Pipes.Attoparsec
import qualified Pipes.Prelude                    as Pipes

main :: IO ()
main = do
  home <- getEnv "HOME"
  let maildir = fromString (home ++ "/Maildir/new")
  Just (Right taglist) <- readTags (fromString (home ++ "/.restless-mail-tags"))
  summaries <- Seq.sort <$> runSafeT (scan maildir)
  let tagmap = Map.fromList taglist
--  let senders = summaries & fmap summaryFrom & toList & Set.fromList & toList & Seq.fromList & Seq.sort

  files <- forM (toList summaries) $ \x -> do
    case Map.lookup (summaryFrom x) tagmap of
      Nothing  -> pure mempty
      Just tag -> mailToFiles tag x

  metadata <- Git.Metadata "x" "y" "z" <$> Git.now
  Git.save (home ++ "/mail") metadata (mconcat files)

  return ()

mailToFiles :: Text -> Summary -> IO (Set Git.File)
mailToFiles tag summary = do
  bytes <- BS.readFile (summaryPath summary)
  return . Set.fromList $ f summary bytes
  where
    f :: Summary -> ByteString -> [Git.File]
    f Summary {..} bytes =
      [ Git.File (Git.Path (subdir tag summary) "mail") bytes
      , Git.File (Git.Path (subdir tag summary) "from")
          (encodeUtf8 summaryFrom)
      ]

subdir :: Text -> Summary -> [ByteString]
subdir tag x =
    [ encodeUtf8 . pack $ formatTime defaultTimeLocale "%Y-%m" (summaryDate x)
    , encodeUtf8 $ mconcat
      [ tag , " "
      , pack $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" (summaryDate x)
      , " " , Text.replace "/" "-SLASH-" (summarySubject x)
      ]
    ]

displaySummary :: Summary -> LazyText.Text
displaySummary x = format "{}\n{}\n{}\n{}\n"
  ( summaryId x
  , show (summaryDate x)
  , summaryFrom x
  , summarySubject x
  )

scan :: MonadSafe m => OS.FilePath -> m (Seq Summary)
scan maildir =
  Pipes.fold (Seq.|>) mempty id .
    for (every (childOf maildir)) $ \path -> do
      result <- liftIO (mail path)
      case result of
        Just (Right x) ->
          yield x
        _ ->
          return ()

idFromPath :: OS.FilePath -> Text
idFromPath x =
  case OS.toText (OS.filename x) of
    Left _ -> error "dumb"
    Right s -> fst (Text.breakOn ":" s)

mail
  :: OS.FilePath
  -> IO (Maybe (Either Pipes.Attoparsec.ParsingError Summary))
mail path =
  withFile path ReadMode $ \h ->
    do x <- evalStateT
              (Pipes.Attoparsec.parse
                 (parseSummary (idFromPath path)
                    (OS.encodeString path)))
              (fromHandle h)
       return x

readTags
  :: OS.FilePath
  -> IO (Maybe (Either Pipes.Attoparsec.ParsingError [(Text, Text)]))
readTags path =
  withFile path ReadMode $ \h ->
    evalStateT
      (Pipes.Attoparsec.parse parseTags)
      (fromHandle h)
