{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving, TypeApplications
             #-}

{- |
   Module      : Main
   Description : Property-based tests
   Copyright   : Ivan Lazar Miljenovic, Patryk Zadarnowski
   License     : MIT
   Maintainer  : Ivan.Miljenovic@gmail.com



 -}
module Main (main) where

import Data.Theseus

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck       (Arbitrary(..))

import Data.Int
import Data.Proxy
import Data.Storable.Endian (BigEndian(..), LittleEndian(..))
import Data.Word

--------------------------------------------------------------------------------

-- Note: tests are _not_ exhaustive for each type as for the most part
-- they are repetitive; only sufficient tests so as to cover all
-- possibilities are included.

main :: IO ()
main = hspec $
  describe "decode . encode = id" $ do
    describe "Explicit instances" $ do
      prop "Int8"               (encodeDecode (Proxy @Int8))
      prop "Word8"              (encodeDecode (Proxy @Word8))
      prop "Int16"              (encodeDecode (Proxy @Int16))
      prop "Word32"             (encodeDecode (Proxy @Word32))
      prop "Double"             (encodeDecode (Proxy @Double))
      prop "Char"               (encodeDecode (Proxy @Char))
    describe "Endian wrappers" $ do
      prop "BE Word16"          (encodeDecode (Proxy @(BigEndian Word16)))
      prop "LE Word16"          (encodeDecode (Proxy @(LittleEndian Word16)))
    describe "Constructors without arguments" $ do
      prop "()"                 (encodeDecode (Proxy @()))
      prop "Proxy Int"          (encodeDecode (Proxy @(Proxy Int)))
    describe "Sum types" $ do
      prop "Bool"               (encodeDecode (Proxy @Bool))
      -- Making sure third constructor works
      prop "Ordering"           (encodeDecode (Proxy @Ordering))
      prop "Maybe Word8"        (encodeDecode (Proxy @(Maybe Word8)))
      prop "Either Char Bool"   (encodeDecode (Proxy @(Either Char Bool)))
      prop "[Char]"             (encodeDecode (Proxy @[Char]))
    describe "Product types" $ do
      prop "(Word32, Ordering)" (encodeDecode (Proxy @(Word32, Ordering)))
      prop "((), Bool, Word8)"  (encodeDecode (Proxy @((), Bool, Word8)))

--------------------------------------------------------------------------------

encodeDecode :: (Eq a, Theseus a) => Proxy a -> a -> Bool
encodeDecode _ a = either (const False) (a==) (unravel (ravel a))

--------------------------------------------------------------------------------

-- Since storable-endian doesn't define these...

deriving instance (Eq a) => Eq (BigEndian a)
deriving instance (Eq a) => Eq (LittleEndian a)

deriving instance (Show a) => Show (BigEndian a)
deriving instance (Show a) => Show (LittleEndian a)

deriving instance (Arbitrary a) => Arbitrary (BigEndian a)
deriving instance (Arbitrary a) => Arbitrary (LittleEndian a)

--------------------------------------------------------------------------------

-- QC 2.9 doesn't have an Arbitrary instance for Proxy...

instance Arbitrary (Proxy a) where
  arbitrary = pure Proxy

  shrink _ = []
