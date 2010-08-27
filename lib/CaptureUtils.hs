module NXT.CaptureUtils where

import Control.Exception
import qualified Data.ByteString.Lazy.Char8 as C
import System.IO

import NXT.CaptureTypes

slurpInput :: Read a => Handle -> CaptureBuffer -> IO ([a], CaptureBuffer)
slurpInput = slurpInput' []
  where slurpInput' datalist h buffer@(CaptureBuffer capacity before) = do
          ready <- hReady h
          if not ready
            then return (datalist, buffer)
            else do
              after <- C.hGetNonBlocking h (capacity - (fromIntegral $ C.length before))
              let new = before `C.append` after
                  chars = C.unpack new
                  (ds, rest) = readInput chars
              if null ds
                then if (fromIntegral $ C.length new) == capacity
                       then error "Invalid data from input"
                       else return (datalist, (CaptureBuffer capacity new))
                else do
                  let rest' = assert ((fromIntegral $ C.length rest') <= capacity) $ C.pack rest
                  slurpInput' (ds ++ datalist) h (CaptureBuffer capacity rest')

readInput :: Read a => String -> ([a], String)
readInput = readInput' []
  where readInput' datalist ""     = (datalist, "")
        readInput' datalist string = case reads string of
                                       [(x, rest)] -> readInput' (x:datalist) rest
                                       []          -> (datalist, string) -- we probably do not have enough data to read data properly
                                       _           -> error "Ambiguous parse from input"
