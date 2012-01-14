module Robotics.NXT.Basic where

import Control.Applicative
import Control.Monad.State hiding (state, runState)
import qualified Data.ByteString.Lazy as B
import Data.IORef
import Data.Maybe
import Data.Time.Clock.POSIX
import System.FilePath
import System.IO
import Test.HUnit

import Robotics.NXT
import Robotics.NXT.Remote
import Robotics.NXT.Sensor.Ultrasonic

basicTests :: IORef NXTInternals -> [Test]
basicTests ref = map (\x -> x ref) [
    testDeviceInfo,
    testProgramUpload,
    testDeviceInit,
    testOutputState,
    testInputMode,
    testUltrasonicSensor
  ]

keepAliveAfter :: Int
keepAliveAfter = 4 * 60 -- 4 minutes (in seconds)

-- Maybe sends a keep alive packet - if more than keepAliveAfter seconds passed from a previous one
maybeKeepAlive :: NXT ()
maybeKeepAlive = do
  lka <- getLastKeepAliveTime
  let lka' = fromMaybe 0 lka
  current <- liftIO getPOSIXTime
  if current - lka' > fromIntegral keepAliveAfter
    then keepAlive
    else return () -- it is not yet time to send a keep alive packet

testNXT :: IORef NXTInternals -> NXT a -> IO a
testNXT ref t = do
  let t' = do r <- t
              maybeKeepAlive
              return r
  nxt <- readIORef ref
  (res, nxt') <- runNXT t' nxt
  writeIORef ref nxt'
  return res

testDeviceInfo :: IORef NXTInternals -> Test
testDeviceInfo ref = TestLabel "testDeviceInfo" $ TestCase $ do
  (DeviceInfo name address _ _) <- testNXT ref getDeviceInfo
  assertBool "name" (not . null $ name)
  putStrLn $ "NXT Name: " ++ name
  assertBool "address" (not . null $ address)
  putStrLn $ "NXT Address: " ++ address

remoteProgramFilename :: String
remoteProgramFilename = "remote/remote.nxc"

testProgramUpload :: IORef NXTInternals -> Test
testProgramUpload ref = TestLabel "testProgramUpload" $ TestCase $ do
  testNXT ref $ do
    stopProgramConfirm
    h <- liftIO $ openBinaryFile remoteProgramFilename ReadMode
    size <- liftIO $ hFileSize h
    content <- liftIO $ B.unpack <$> B.hGetContents h
    let filename = takeFileName remoteProgramFilename
    deleteConfirm filename
    h' <- openWrite filename (fromIntegral size)
    mapM_ (write h') $ chunk 61 content
    close h'
  where chunk _ [] = [[]]
        chunk n xs = y1 : chunk n y2
          where (y1, y2) = splitAt n xs

testDeviceInit :: IORef NXTInternals -> Test
testDeviceInit ref = TestLabel "testDeviceInit" $ TestCase $ do
  testNXT ref $ do
    startRemoteProgram
    mapM_ resetInputScaledValue [One ..]
    mapM_ (`resetMotorPosition` AbsolutePosition) [A ..]
    mapM_ (`resetMotorPosition` RelativePosition) [A ..]
    mapM_ (`resetMotorPosition` InternalPosition) [A ..]
    setOutputStateConfirm A 0 [MotorOn, Brake] RegulationModeIdle 0 MotorRunStateRunning 0
    setOutputStateConfirm B 0 [MotorOn, Brake] RegulationModeIdle 0 MotorRunStateRunning 0
    setOutputStateConfirm C 0 [MotorOn, Brake] RegulationModeIdle 0 MotorRunStateRunning 0

waitfor :: NXT (Bool, Int) -> NXT Bool
waitfor cond = waitfor' []
  where window        = 6
        allowed       = 10
        waitfor' prev = do
          (c, r) <- cond
          let prev'  = take window $ (abs r):prev
              prev'' = derive $ prev'
              speed  = (sum prev'') `div` (length prev'')
          if c
            then return True
            else if length prev' < window
                   then waitfor' prev'
                   else if speed < allowed -- speed should not fall under allowed threshold
                          then return False
                          else waitfor' prev'
          where derive xs = zipWith (-) xs (tail xs) -- xs is a reversed list

testOutputState :: IORef NXTInternals -> Test
testOutputState ref = TestLabel "testOutputState" $ TestCase $ do
  testNXT ref $ do
    setOutputStateConfirm A 75 [MotorOn, Brake, Regulated] RegulationModeMotorSpeed 0 MotorRunStateRunning 1000
    successful <- waitfor $ do
      OutputState _ _ _ _ _ state _ _ tachoCount _ <- getOutputState A
      return (state == MotorRunStateIdle, fromIntegral tachoCount)
    setOutputStateConfirm A 0 [MotorOn, Brake] RegulationModeIdle 0 MotorRunStateRunning 0
    liftIO $ assertBool "not successful waitfor" successful
    OutputState outputPort outputPower outputMode regulationMode turnRatio runState tachoLimit tachoCount _ _ <- getOutputState A
    liftIO $ do
      assertEqual "outputPort" A outputPort
      assertEqual "outputPower" 0 outputPower
      assertEqual "outputMode" [MotorOn, Brake] outputMode
      assertEqual "regulationMode" RegulationModeIdle regulationMode
      assertEqual "turnRatio" 0 turnRatio
      assertEqual "runState" MotorRunStateRunning runState
      assertEqual "tachoLimit" 0 tachoLimit
      assertBool ("tachoCount !~ 1000: " ++ show tachoCount) (tachoCount > 700 && tachoCount < 1300)

testInputMode :: IORef NXTInternals -> Test
testInputMode ref = TestLabel "testInputMode" $ TestCase $ do
  InputValue inputPort valid _ sensorType sensorMode _ normalizedADValue scaledValue _ <- testNXT ref $ do
    setInputModeConfirm One Switch BooleanMode
    getInputValues One
  assertEqual "inputPort" One inputPort
  assertBool "not valid" valid
  assertEqual "sensorType" Switch sensorType
  assertEqual "sensorMode" BooleanMode sensorMode
  assertBool ("normalizedADValue not in range [0, 1023]: " ++ show normalizedADValue) (normalizedADValue >= 0 && normalizedADValue <= 1023)
  assertEqual "scaledValue" 0 scaledValue

testUltrasonicSensor :: IORef NXTInternals -> Test
testUltrasonicSensor ref = TestLabel "testUltrasonicSensor" $ TestCase $ do
  measurement <- testNXT ref $ do
    usInit Two
    version <- usGetVersion Two
    liftIO $ assertEqual "version" "V1.0" version
    vendor <- usGetVendorID Two
    liftIO $ assertEqual "vendor" "LEGO" vendor
    device <- usGetDeviceID Two
    liftIO $ assertEqual "device" "Sonar" device
    units <- usGetMeasurementUnits Two
    liftIO $ assertEqual "units" "10E-2m" units
    usSetMode Two ContinuousMeasurement
    mode <- usGetMode Two
    liftIO $ assertEqual "mode" ContinuousMeasurement mode
    usSetMode Two SingleShot
    measurement <- usGetMeasurement Two 0
    usSetMode Two Off
    mode' <- usGetMode Two
    liftIO $ assertEqual "mode" Off mode'
    return measurement
  putStrLn $ "Ultrasonic sensor measurement: " ++ (show measurement)
