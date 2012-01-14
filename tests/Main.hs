module Main where

import Control.Exception
import Control.Monad
import Data.Maybe
import Data.List
import Data.IORef
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO

import Test.Framework
import Test.Framework.Providers.HUnit

import Robotics.NXT
import Robotics.NXT.Basic

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
  (opts, otherArgs) <- case getOpt Permute options args of
                         (o, otherArgs, []) -> return (o, otherArgs)
                         (_, _, errs)       -> do
                           hPutStrLn stderr $ "Error(s):\n" ++ concat errs ++ "\n" ++ usage
                           exitWith $ ExitFailure 1

  when (Help `elem` opts) $ do
    putStrLn "Runs the NXT package tests.\n"

    putStrLn usage
    exitWith ExitSuccess

  let Device device = fromMaybe (Device defaultDevice) . find isDevice $ opts

  putStrLn "Please connect a motor to port A, a switch sensor to port 1, an ultrasonic sensor to port 2, and press enter key to continue."

  _ <- try getLine :: IO (Either IOException String)

  bracket
    (initialize device >>= newIORef)
    (\ref -> do
       nxt <- readIORef ref
       terminate nxt
    )
    (\ref -> defaultMainWithArgs (tests ref) otherArgs)
 
tests :: IORef NXTInternals -> [Test]
tests ref = [
    testGroup "Basic Tests" (concatMap hUnitTestToTests (basicTests ref))
  ]
