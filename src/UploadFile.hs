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
  stopProgram
  mapM_ uploadFile args
    where uploadFile file = do
            io $ putStrLn $ "Uploading " ++ file
            h <- io $ openBinaryFile file ReadMode
            size <- io $ hFileSize h
            content <- io $ hGetContents h
            let filename = takeFileName file
            delete filename
            h' <- openWrite filename (fromIntegral size)
            mapM_ (write h' . stringToData) $ chunk 61 content
            close h'
          chunk _ [] = [[]]
          chunk n xs = y1 : chunk n y2
            where (y1, y2) = splitAt n xs
