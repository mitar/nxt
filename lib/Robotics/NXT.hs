{-|
This module defines an interface over Bluetooth to a NXT brick as defined in Lego Mindstorms NXT Bluetooth Developer Kit,
Appendix 1 - Communication protocol and Appendix 2 - Direct commands. It also defines some additional functions not available
directly otherwise.
-}

module Robotics.NXT (
  module Robotics.NXT.Protocol,
  module Robotics.NXT.BluetoothUtils,
  module Robotics.NXT.Types,
  module Robotics.NXT.Errors
) where

import Robotics.NXT.BluetoothUtils
import Robotics.NXT.Errors
import Robotics.NXT.Protocol
import Robotics.NXT.Types
