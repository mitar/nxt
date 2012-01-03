
module Robotics.NXT.Basic where

import Robotics.NXT
import Test.HUnit

import Control.Concurrent (threadDelay)
import Data.IORef

basicTests :: IORef NXTInternals -> [Test]
basicTests d= map (\x->x d) [testDeviceInfo,testOutputState]
--

delay :: IO()
delay =threadDelay $ 1000000 * 30

testNXT :: IORef NXTInternals ->  NXT a -> IO a
testNXT ref f=do
        i<-readIORef ref
        (a,i')<-runNXT f i
        writeIORef ref i'
        return a


testDeviceInfo :: IORef NXTInternals -> Test
testDeviceInfo ref= TestLabel "testDeviceInfo" (TestCase (do
        (DeviceInfo name address _ _)<-testNXT ref getDeviceInfo
        assertBool "empty name" (not $ null name)
        putStrLn ("NXT Name: "++name)
        assertBool "empty address" (not $ null address)
        putStrLn ("NXT Address: "++address)
        ))
        
testOutputState :: IORef NXTInternals -> Test
testOutputState ref= TestLabel "testOutputState" (TestCase (do
       (OutputState port power modes reg ratio runstate limit count block rotation) <- testNXT ref (do
                setOutputStateConfirm A 75 [MotorOn,Brake] RegulationModeMotorSpeed 0 MotorRunStateRunning 360
                getOutputState A
                )
       assertEqual "not A port" A port
       ))