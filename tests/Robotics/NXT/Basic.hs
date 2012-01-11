module Robotics.NXT.Basic where

import Robotics.NXT
import Test.HUnit

import Control.Concurrent (threadDelay)
import Data.IORef

basicTests :: IORef NXTInternals -> [Test]
basicTests d= map (\x->x d) [testDeviceInfo,testOutputState,testInputMode]
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
       (OutputState port power modes reg ratio _ limit count _ _) <- testNXT ref (do
                setOutputStateConfirm A 75 [MotorOn,Brake] RegulationModeMotorSpeed 0 MotorRunStateRunning 360
                getOutputState A
                )
       assertEqual "not A port" A port
       assertEqual "not 75 power" 75 power
       assertEqual "not modes" [MotorOn,Brake] modes
       assertEqual "not regulation" RegulationModeMotorSpeed reg
       assertEqual "not 0 ratio" 0 ratio
       assertEqual "not 360 limit" 360 limit
       assertBool "count>0" (count>0)
       ))
       
testInputMode :: IORef NXTInternals -> Test
testInputMode ref= TestLabel "testInputMode" (TestCase (do
        InputValue port valid calibrated stype smode _ normV scalV _<-testNXT ref (do
                setInputModeConfirm One Switch BooleanMode
                getInputValues One
                )
        assertEqual "not port 1" One port
        assertBool "not valid" valid
        assertBool "calibrated" (not calibrated)
        assertEqual "not switch" Switch stype
        assertEqual "not boolean" BooleanMode smode
        assertBool "normalized not in range" (normV>=0 && normV<1024)
        assertEqual "scaled not 0" 0 scalV
        ))
