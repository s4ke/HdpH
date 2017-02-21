module Control.Parallel.HdpH.SerialUtil(
    deserialize
) where

import GHC.Packing.Core
import GHC.Packing.Type

import System.IO.Unsafe (unsafePerformIO)

deserial :: Serialized a -> a
deserial = unsafePerformIO . deserialize

serial :: a -> Serialized a
serial = unsafePerformIO . trySerialize

