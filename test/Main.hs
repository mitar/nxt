
module Main where

import Robotics.NXT
import Robotics.NXT.Basic

import Control.Exception
import Control.Monad.State.Class

import Test.Framework (defaultMainWithArgs, testGroup,Test)
import Test.Framework.Providers.HUnit


import System.Environment ( getArgs)
import Data.IORef
import Control.Monad.IO.Class (liftIO)

main :: IO()
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
tests ref= [testGroup "Basic Tests" (concatMap hUnitTestToTests (basicTests ref))
         ]
         
