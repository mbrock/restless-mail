module Restless.Mail where

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

data Summary = Summary
  { summaryId      :: Text
  , summaryPath    :: Prelude.FilePath
  , summaryFrom    :: Text
  , summaryDate    :: UTCTime
  , summarySubject :: Text
  } deriving (Show, Eq, Generic)

instance Ord Summary where
  compare = flip (comparing summaryDate)

data HeaderField = HeaderField
  { headerName :: ByteString
  , headerBody :: ByteString
  } deriving (Show, Ord, Eq)
