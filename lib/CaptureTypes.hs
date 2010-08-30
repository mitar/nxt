{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module NXT.CaptureTypes where

import qualified Data.ByteString.Lazy.Char8 as C
import Data.Fixed
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Numeric
import System.IO
import System.Process

-- TODO Change to record
data Robot = Robot
    Double -- X coordinate of a robot
    Double -- Y coordinate of a robot
    Double -- direction of a robot in radians from positive X axis
    POSIXTime -- capture timestamp in microseconds (as determined by a clock of the remote process)
    POSIXTime -- processed timestamp in microseconds (as determined by a clock of the remote process)
    POSIXTime -- receive timestamp in microseconds (as determined by a clock of this process)
  deriving (Eq, Read, Show)

-- Approximate but larger than possible length of Robot data type text description
-- We assume that total length of POSIXTime is at most three times its precision, and 42 characters for data type format structure itself
maxRobotDescLength :: Int
#if __GLASGOW_HASKELL__ <= 610
maxRobotDescLength = length "Robot" + 3 * (floatDigits (undefined :: Double)) + 3 * 3 * (length . show $ resolution (undefined :: E12)) + 42
#else
maxRobotDescLength = length "Robot" + 3 * (floatDigits (undefined :: Double)) + 3 * 3 * (length . show $ resolution (undefined :: Pico)) + 42
#endif

instance Read NominalDiffTime where -- POSIXTime is NominalDiffTime
  readsPrec _ = readFloat

type Capacity = Int
data CaptureBuffer = CaptureBuffer Capacity C.ByteString

data Capture = Capture Handle ProcessHandle CaptureBuffer
