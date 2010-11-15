module Main (
  main
) where

import Control.Exception
import Control.Monad.State
import Data.Maybe
import Data.List
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import System.FilePath

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
  let header = programName ++ " [option ...] <file ...>" ++ "\n\nOptions:"
      usage  = "Usage:\n" ++ usageInfo header options
  
  args <- getArgs
  (opts, files) <- case getOpt Permute options args of
                     (o, fs, [])  -> return (o, fs)
                     (_, _, errs) -> do
                       hPutStrLn stderr $ "Error(s):\n" ++ concat errs ++ "\n" ++ usage
                       exitWith $ ExitFailure 1
  
  when (Help `elem` opts) $ do
    putStrLn "Uploads files to NXT brick.\n"
    putStrLn usage
    exitWith ExitSuccess
  
  when (null files) $ do
    hPutStrLn stderr $ "Error(s):\nno files to upload specified\n" ++ "\n" ++ usage
    exitWith $ ExitFailure 1
  
  let Device device = fromMaybe (Device defaultDevice) . find isDevice $ opts
  
  bracket (initialize device) terminate (evalStateT (uploadFiles files))

uploadFiles :: [String] -> NXT ()
uploadFiles args = do
  stopProgramConfirm
  mapM_ uploadFile args
    where uploadFile file = do
            liftIO $ putStrLn $ "Uploading " ++ file
            h <- liftIO $ openBinaryFile file ReadMode
            size <- liftIO $ hFileSize h
            content <- liftIO $ hGetContents h
            let filename = takeFileName file
            deleteConfirm filename
            h' <- openWrite filename (fromIntegral size)
            mapM_ (write h' . stringToData) $ chunk 61 content
            close h'
          chunk _ [] = [[]]
          chunk n xs = y1 : chunk n y2
            where (y1, y2) = splitAt n xs
