module Control.Parallel.HdpH.SerialUtil(
    deserial,
    serial
) where

import GHC.Packing.Core
import GHC.Packing.Type

import System.IO.Unsafe (unsafePerformIO)

deserial :: Serialized a -> a
deserial = unsafePerformIO . deserialize
{-# NOINLINE deserial #-}

serial :: a -> Serialized a
serial = unsafePerformIO . trySerialize
{-# NOINLINE serial #-}

