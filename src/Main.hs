{-# Language TemplateHaskell #-}
{-# Language DataKinds #-}
{-# Language TypeOperators #-}
{-# Language DeriveGeneric #-}
{-# Language OverloadedStrings #-}
{-# Language LambdaCase #-}

module Main where

import Restless.Mail
import Restless.Mail.Parser

import Data.DirStream     (childOf)
import Data.Foldable      (forM_)
import Data.Sequence      (Seq)
import Data.String        (fromString)
import Data.Text          (Text)
import Data.Text.Format   (format)
import Data.Thyme.Format  (formatTime)
import Filesystem         (IOMode (ReadMode), withFile)
import Pipes hiding       (Proxy)
import Pipes.ByteString   (fromHandle)
import Pipes.Parse        (evalStateT)
import Pipes.Safe         (MonadSafe, runSafeT)
import System.Directory   (createDirectoryIfMissing, copyFile)
import System.Environment (getEnv)
import System.Locale      (defaultTimeLocale)

import qualified Filesystem.Path.CurrentOS        as OS
import qualified Data.Map                         as Map
import qualified Data.Sequence                    as Seq
import qualified Data.Text                        as Text
import qualified Data.Text.Lazy                   as LazyText
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

  forM_ summaries $ \x -> do
    case Map.lookup (summaryFrom x) tagmap of
      Nothing -> pure ()
      Just tag -> do
        let dir = home ++ "/mail/" ++ subdir tag x
        createDirectoryIfMissing True dir
        System.Directory.copyFile (summaryPath x) (dir ++ "/" ++ "mail")

  return ()

subdir :: Text -> Summary -> String
subdir tag x =
  formatTime defaultTimeLocale "%Y-%m" (summaryDate x) ++ "/" ++ Text.unpack tag
    ++ " " ++ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" (summaryDate x)
    ++ " " ++ Text.unpack (Text.replace "/" "-SLASH-" (summarySubject x))

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
