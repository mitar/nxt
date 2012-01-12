module Main (
  main
) where

import Control.Monad.State
import Data.Maybe
import Data.List
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import Text.Printf

import Robotics.NXT

data Option = Help | Device FilePath deriving (Eq, Show)

isDevice :: Option -> Bool
isDevice (Device _) = True
isDevice _          = False

options :: [OptDescr Option]
options = [
    Option "h" ["help"] (NoArg Help) "show this help",
    Option "d" ["device"] (ReqArg Device "filename") "serial port device"
  ]

main :: IO ()
main = do
  programName <- getProgName
  let header = programName ++ " [option ...]" ++ "\n\nOptions:"
      usage  = "Usage:\n" ++ usageInfo header options
  
  args <- getArgs
  opts <- case getOpt Permute options args of
                 (o, [], [])  -> return o
                 (_, _, [])   -> do
                   hPutStrLn stderr $ "Error(s):\n" ++ "excess argument(s)\n\n" ++ usage
                   exitWith $ ExitFailure 1
                 (_, _, errs) -> do
                   hPutStrLn stderr $ "Error(s):\n" ++ concat errs ++ "\n" ++ usage
                   exitWith $ ExitFailure 1
  
  when (Help `elem` opts) $ do
    putStrLn "Remotely shutdowns a NXT brick.\n"
    putStrLn usage
    exitWith ExitSuccess
  
  let Device device = fromMaybe (Device defaultDevice) . find isDevice $ opts
  
  withNXT device $ do
    DeviceInfo name btaddress btstrength flashfree <- getDeviceInfo
    Version (FirmwareVersion fmajor fminor) (ProtocolVersion pmajor pminor) <- getVersion
    battery <- getBatteryLevel
    rechargeable <- isBatteryRechargeable
    sleeplimit <- getSleepTimeout
    let fversion = printf "%d.%02d" fmajor fminor :: String
        pversion = printf "%d.%02d" pmajor pminor :: String
    liftIO $ hPutStrLn stderr $ printf "Connected to %s at %s." name btaddress
    liftIO $ hPutStrLn stderr $ printf "Running firmware version %s with protocol version %s." fversion pversion
    liftIO $ hPutStrLn stderr $ printf "Battery level: %f V (%s)" (realToFrac battery :: Double) (if rechargeable then "rechargeable" else "not rechargeable")
    liftIO $ hPutStrLn stderr $ printf "Free space: %d bytes" flashfree
    liftIO $ hPutStrLn stderr $ printf "Signal strength: %d" btstrength
    liftIO $ hPutStrLn stderr $ printf "Sleep time limit: %f s" (realToFrac sleeplimit :: Double)
