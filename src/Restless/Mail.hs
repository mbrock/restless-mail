module Restless.Mail where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Thyme.Clock (UTCTime)
import Data.Thyme.Format ()
import Data.Ord (comparing)
import Data.Set (Set)
import GHC.Generics (Generic)

import qualified Data.Set as Set

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

data Archive = Archive (Set Summary)
