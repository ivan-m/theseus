{-# LANGUAGE ConstraintKinds, KindSignatures, OverloadedStrings, RankNTypes,
             TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

{- |
   Module      : Main
   Description : Compare binary serialisation libraries
   Copyright   : Ivan Lazar Miljenovic, Patryk Zadarnowski
   License     : MIT
   Maintainer  : Ivan.Miljenovic@gmail.com



 -}
module Main (main) where

import Data.Theseus
import Data.Theseus.SampleTypes

import qualified Data.Binary    as B
import qualified Data.Serialize as C

import Test.HUnit (assertEqual)
import TestBench

import           Control.DeepSeq      (NFData(..))
import           Data.ByteString      (ByteString)
import qualified Data.ByteString.Lazy as LBS
import           Data.Proxy           (Proxy(..))
import           Data.Storable.Endian (BigEndian(..), LittleEndian(..))
import           Data.Word            (Word8)
import           GHC.Exts             (Constraint)

--------------------------------------------------------------------------------

main :: IO ()
main =
  testBench (mapM_ (uncurry compareWithValue) samples)

compareWithValue :: String -> OuterStructure -> TestBench
compareWithValue lbl a = compareFunc ("Comparing " ++ lbl ++ " values")
                                     (`withLibrary` (encodeDecode a))
                                     (testWith (assertEqual "Should decode original value" (Just a))
                                      `mappend` benchNormalForm)
                                     (mapM_ (comp =<< show) [minBound .. maxBound])

--------------------------------------------------------------------------------

data Library = Theseus
             | Binary
             | Cereal
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

withLibrary :: Library -> (forall l. (Serialisation l) => Proxy l -> k) -> k
withLibrary Theseus k = (k (Proxy @Theseus))
withLibrary Binary  k = (k (Proxy @B.Binary))
withLibrary Cereal  k = (k (Proxy @C.Serialize))

class Serialisation (l :: * -> Constraint) where

  encode :: Proxy l -> OuterStructure -> ByteString

  decode :: Proxy l -> ByteString -> Maybe OuterStructure

instance Serialisation Theseus where

  encode _ = ravel

  decode _ = either (const Nothing) Just . unravel

encodeDecode :: (Serialisation l) => OuterStructure -> Proxy l -> Maybe OuterStructure
encodeDecode a p = decode p (encode p a)

--------------------------------------------------------------------------------

instance Serialisation B.Binary where

  encode _ = LBS.toStrict . B.encode

  decode _ bs = case B.decodeOrFail (LBS.fromStrict bs) of
                  Right ("", _, a) -> Just a
                  _                -> Nothing

instance (B.Binary a) => B.Binary (BigEndian a)
instance (B.Binary a) => B.Binary (LittleEndian a)
instance (B.Binary a) => B.Binary (InnerStructure a)
instance                 B.Binary OuterStructure

--------------------------------------------------------------------------------

instance Serialisation C.Serialize where

  encode _ = C.encode

  decode _ = either (const Nothing) Just . C.decode

instance (C.Serialize a) => C.Serialize (BigEndian a)
instance (C.Serialize a) => C.Serialize (LittleEndian a)
instance (C.Serialize a) => C.Serialize (InnerStructure a)
instance                    C.Serialize OuterStructure

--------------------------------------------------------------------------------

instance               NFData Ordering
instance (NFData a) => NFData (BigEndian a)
instance (NFData a) => NFData (LittleEndian a)
instance (NFData a) => NFData (InnerStructure a)
instance               NFData OuterStructure

samples :: [(String, OuterStructure)]
samples = [ ("small",  smallOuter)
          , ("medium", mediumOuter)
          , ("large",  largeOuter)
          ]

smallInner :: InnerStructure Word8
smallInner = ISTwo

smallOuter :: OuterStructure
smallOuter = OS True
                (Right ())
                smallInner
                Nothing

mediumInner :: InnerStructure Word8
mediumInner = ISOne 0 ((), LT, 1.23456)

mediumOuter :: OuterStructure
mediumOuter = OS True
                 (Left minBound)
                 mediumInner
                 (Just "medium")

largeInner :: InnerStructure Word8
largeInner = ISThree { _three1 = "Is this the real life?"
                     , _three2 = (253, BE 6243, LE 9834234, 23409834)
                     , _three3 = "Is this just fantasy?"
                     }

largeOuter :: OuterStructure
largeOuter = OS False
                (Left maxBound)
                largeInner
                (Just "Scaramouche! Scaramouche! Will you do the Fandango?!?\n\
                      \Thunderbolt and lightning, very very frightening me!")
