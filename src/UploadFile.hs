module UploadFile where

import Control.Exception
import Control.Monad.State
import System.Environment
import System.IO
import System.FilePath

import NXT.NXT
import NXT.Data
import NXT.Types

upload :: IO ()
upload = do
  args <- getArgs
  bracket initialize terminate (evalStateT (uploadFiles args))

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
