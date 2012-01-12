module Main where

import Control.Exception
import Control.Monad.Trans ()
import Data.IORef
import System.Environment

import Test.Framework
import Test.Framework.Providers.HUnit

import Robotics.NXT
import Robotics.NXT.Basic

main :: IO ()
main = do
  (device:args) <- getArgs
  bracket (do
                i<-initialize device
                newIORef i)
        (\ref->do
                i'<-readIORef ref
                terminate i')
        (\ref->defaultMainWithArgs (tests ref) args)
 
--  withNXT device (do
--        i<-get
--        ref<-liftIO $ newIORef i
--        liftIO $ defaultMainWithArgs (tests ref) args
--        i'<-liftIO $ readIORef ref
--        return ()
--        )

tests :: IORef (NXTInternals) -> [Test]
tests ref = [
    testGroup "Basic Tests" (concatMap hUnitTestToTests (basicTests ref))
  ]
