module Shutdown (
  shutdown
) where

import Control.Exception
import Control.Monad.State
import Data.Maybe
import Data.List
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO

import qualified NXT.NXT as NXT

data Option = Help | Device FilePath deriving (Eq, Show)

isDevice :: Option -> Bool
isDevice (Device _) = True
isDevice _          = False

options :: [OptDescr Option]
options = [
    Option "h" ["help"] (NoArg Help) "show this help",
    Option "d" ["device"] (ReqArg Device "filename") "serial port device"
  ]

shutdown :: IO ()
shutdown = do
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
    putStrLn usage
    exitWith ExitSuccess
  
  let Device device = fromMaybe (Device NXT.defaultDevice) . find isDevice $ opts
  
  bracket (NXT.initialize device) NXT.terminate (evalStateT NXT.shutdown)
