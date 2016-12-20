{-# LANGUAGE DeriveAnyClass, DeriveGeneric, StandaloneDeriving, TypeApplications
             #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

{- |
   Module      : Main
   Description : Property-based tests
   Copyright   : Ivan Lazar Miljenovic
   License     : MIT
   Maintainer  : Ivan.Miljenovic@gmail.com



 -}
module Main (main) where

import Data.Theseus
import Data.Theseus.SampleTypes

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck           (Arbitrary(..), arbitraryBoundedEnum,
                                  genericShrink, oneof)
import Test.QuickCheck.Instances ()

import           Data.ByteString      (ByteString)
import qualified Data.ByteString      as B
import           Data.Int
import           Data.Proxy
import           Data.Storable.Endian (BigEndian(..), LittleEndian(..))
import           Data.Word
import           GHC.Generics

--------------------------------------------------------------------------------

-- Note: tests are _not_ exhaustive for each type as for the most part
-- they are repetitive; only sufficient tests so as to cover all
-- possibilities are included.

main :: IO ()
main = hspec $ do
  describe "decode . encode = id" $ do
    describe "Explicit instances" $ do
      prop "Int8"               (encodeDecode (Proxy @Int8))
      prop "Word8"              (encodeDecode (Proxy @Word8))
      prop "Int16"              (encodeDecode (Proxy @Int16))
      prop "Word32"             (encodeDecode (Proxy @Word32))
      prop "Double"             (encodeDecode (Proxy @Double))
      prop "Char"               (encodeDecode (Proxy @Char))
      prop "ByteString"         (encodeDecode (Proxy @ByteString))
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
      prop "10 constructors"    (encodeDecode (Proxy @LotsOfConstructors))
      prop "Maybe Word8"        (encodeDecode (Proxy @(Maybe Word8)))
      prop "Either Char Bool"   (encodeDecode (Proxy @(Either Char Bool)))
      prop "[Char]"             (encodeDecode (Proxy @[Char]))
    describe "Product types" $ do
      prop "(Word32, Ordering)" (encodeDecode (Proxy @(Word32, Ordering)))
      prop "((), Bool, Word8)"  (encodeDecode (Proxy @((), Bool, Word8)))
    describe "Complex nested structure" $ do
      prop "Inner component"    (encodeDecode (Proxy @(InnerStructure Word8)))
      prop "Entire structure"   (encodeDecode (Proxy @OuterStructure))
  describe "Minimality" $ do
    it "Has minimal representation for sum-types" $
       all ((1==) . B.length . ravel) [minBound .. maxBound :: LotsOfConstructors]
    it "Empty constructors take no space" $
       B.null (ravel ())
    it "No overhead for product types" $
       ravel ('a', True, 127 :: Word16) == mconcat [ravel 'a', ravel True, ravel (127 :: Word16)]

--------------------------------------------------------------------------------

encodeDecode :: (Eq a, Theseus a) => Proxy a -> a -> Bool
encodeDecode _ a = either (const False) (a==) (unravel (ravel a))

--------------------------------------------------------------------------------

data LotsOfConstructors = C0 | C1 | C2 | C3 | C4 | C5 | C6 | C7 | C8 | C9
  deriving (Eq, Show, Bounded, Enum, Generic, Theseus)

instance Arbitrary LotsOfConstructors where
  arbitrary = arbitraryBoundedEnum

instance (Arbitrary a) => Arbitrary (InnerStructure a) where
  arbitrary = oneof [ ISOne <$> arbitrary <*> arbitrary
                    , pure ISTwo
                    , ISThree <$> arbitrary <*> arbitrary <*> arbitrary
                    ]

  shrink = genericShrink

instance Arbitrary OuterStructure where
  arbitrary = OS <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

--------------------------------------------------------------------------------

-- Since storable-endian doesn't define these...

instance (Arbitrary a) => Arbitrary (BigEndian a) where
  arbitrary = BE <$> arbitrary

  shrink = map BE . shrink . getBigEndian

instance (Arbitrary a) => Arbitrary (LittleEndian a) where
  arbitrary = LE <$> arbitrary

  shrink = map LE . shrink . getLittleEndian

--------------------------------------------------------------------------------

-- QC 2.9 doesn't have an Arbitrary instance for Proxy...

instance Arbitrary (Proxy a) where
  arbitrary = pure Proxy

  shrink _ = []
