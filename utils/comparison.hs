{-# LANGUAGE ConstraintKinds, DeriveAnyClass, DeriveGeneric, FlexibleInstances,
             KindSignatures, OverloadedStrings, RankNTypes, TypeApplications
             #-}

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
import Data.Theseus.Values      (sizeOfValue)

import qualified Data.Binary     as B
import qualified Data.Binary.Get as B
import qualified Data.Binary.Put as B
import qualified Data.Serialize  as C

import Test.HUnit (assertEqual)
import TestBench

import           Control.DeepSeq       (NFData(..))
import           Data.ByteString       (ByteString)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as CBS
import qualified Data.ByteString.Lazy  as LBS
import           Data.Char             (chr)
import           Data.Proxy            (Proxy(..))
import           Data.Storable.Endian  (BigEndian(..), LittleEndian(..))
import           Data.Word
import           GHC.Exts              (Constraint)
import           GHC.Generics          (Generic)
import           Text.Printf

--------------------------------------------------------------------------------

main :: IO ()
main = do
  testBench $ do
    collection "Comparing encoding speed" $
      compareFuncAll "Grouped Word* values"
                     (`withLibrary` (\p -> map (encode p) bws))
                     (noBenchmarks `mappend` weigh)
    collection "Comparing decoding speed" $
      compareFuncAll "Grouped Word* values"
                     (`withLibrary` (\p -> map (decode p) bss :: [Maybe BenchWord]))
                     (noBenchmarks `mappend` weigh)
    collection "Comparing encoding/decoding speed" $ do
      compareWithValue "Grouped Word*" zeroBenchWord
      collection "Custom structure"
                 (mapM_ (uncurry compareWithValue) samples)

  putStrLn "" -- blank line
  putStrLn "Comparing encoding size"
  compareSizes "Grouped Word*" zeroBenchWord
  mapM_ (uncurry (compareSizes . (++ " custom structure"))) samples
  where
    (bws, bss) = genBenchData 1000000

compareWithValue :: (CanTestWith a) => String -> a -> TestBench
compareWithValue lbl a = compareFuncAll' (lbl ++ " values")
                                         (`withLibrary` (encodeDecode a))
                                         (testWith (assertEqual "Should decode original value" (Just a))
                                          `mappend` (noBenchmarks `mappend` weigh))

compareSizes :: (SerialiseAll a) => String -> a -> IO ()
compareSizes lbl a = do printf "  %s values\n" lbl
                        mapM_ (sizeOf =<< show) libraries
  where
    sizeOf n l = printf "    %-20s %3d bytes\n" n
                                                (BS.length (withLibrary l (`encode`a)))

--------------------------------------------------------------------------------

data Library = Theseus
             | Binary
             | Cereal
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

libraries :: [Library]
libraries = [minBound .. maxBound]

withLibrary :: Library -> (forall l. (Serialisation l) => Proxy l -> k) -> k
withLibrary Theseus k = (k (Proxy @Theseus))
withLibrary Binary  k = (k (Proxy @B.Binary))
withLibrary Cereal  k = (k (Proxy @C.Serialize))

type SerialiseAll a = (Theseus a, B.Binary a, C.Serialize a)

type CanTestWith a = (Eq a, Show a, NFData a, SerialiseAll a)

class Serialisation (l :: * -> Constraint) where

  encode :: (SerialiseAll a) => Proxy l -> a -> ByteString

  decode :: (SerialiseAll a) => Proxy l -> ByteString -> Maybe a

instance Serialisation Theseus where

  encode _ = ravel

  decode _ = either (const Nothing) Just . unravel

encodeDecode :: (Serialisation l, SerialiseAll a) => a -> Proxy l -> Maybe a
encodeDecode a p = decode p (encode p a)

--------------------------------------------------------------------------------

instance Serialisation B.Binary where

  encode _ = LBS.toStrict . B.encode

  decode _ bs = case B.decodeOrFail (LBS.fromStrict bs) of
                  Right ("", _, a) -> Just a
                  _                -> Nothing

instance B.Binary (BigEndian Word16) where
  put = B.putWord16be . getBigEndian

  get = BE <$> B.getWord16be

instance B.Binary (LittleEndian Word16) where
  put = B.putWord16le . getLittleEndian

  get = LE <$> B.getWord16le

instance B.Binary (BigEndian Word32) where
  put = B.putWord32be . getBigEndian

  get = BE <$> B.getWord32be

instance B.Binary (LittleEndian Word32) where
  put = B.putWord32le . getLittleEndian

  get = LE <$> B.getWord32le

instance B.Binary (BigEndian Word64) where
  put = B.putWord64be . getBigEndian

  get = BE <$> B.getWord64be

instance B.Binary (LittleEndian Word64) where
  put = B.putWord64le . getLittleEndian

  get = LE <$> B.getWord64le

instance (B.Binary a) => B.Binary (InnerStructure a)
instance                 B.Binary OuterStructure

--------------------------------------------------------------------------------

instance Serialisation C.Serialize where

  encode _ = C.encode

  decode _ = either (const Nothing) Just . C.decode

instance C.Serialize (BigEndian Word16) where
  put = C.putWord16be . getBigEndian

  get = BE <$> C.getWord16be

instance C.Serialize (LittleEndian Word16) where
  put = C.putWord16le . getLittleEndian

  get = LE <$> C.getWord16le

instance C.Serialize (BigEndian Word32) where
  put = C.putWord32be . getBigEndian

  get = BE <$> C.getWord32be

instance C.Serialize (LittleEndian Word32) where
  put = C.putWord32le . getLittleEndian

  get = LE <$> C.getWord32le

instance C.Serialize (BigEndian Word64) where
  put = C.putWord64be . getBigEndian

  get = BE <$> C.getWord64be

instance C.Serialize (LittleEndian Word64) where
  put = C.putWord64le . getLittleEndian

  get = LE <$> C.getWord64le

instance (C.Serialize a) => C.Serialize (InnerStructure a)
instance                    C.Serialize OuterStructure

--------------------------------------------------------------------------------

instance               NFData Ordering
instance (NFData a) => NFData (BigEndian a)
instance (NFData a) => NFData (LittleEndian a)
instance (NFData a) => NFData (InnerStructure a)
instance               NFData OuterStructure

samples :: [(String, OuterStructure)]
samples = [ ("Small",             smallOuter)
          , ("Medium",            mediumOuter)
          , ("Large",             largeOuter)
          , ("Large (no String)", largeOuterNoString)
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

largeInnerNoString :: InnerStructure Word8
largeInnerNoString = ISThree { _three1 = "Is this the real life?"
                             , _three2 = (253, BE 6243, LE 9834234, 23409834)
                             , _three3 = ""
                             }

largeOuterNoString :: OuterStructure
largeOuterNoString = OS False
                        (Left maxBound)
                        largeInnerNoString
                        (Just "Scaramouche! Scaramouche! Will you do the Fandango?!?\n\
                              \Thunderbolt and lightning, very very frightening me!")

--------------------------------------------------------------------------------

-- | Based upon the comparison benchmark from <https://github.com/erikd/fastpack/blob/master/bench/Bench/Types.hs fastpack>
--
--   The only difference is the usage of explicit BE/LE annotations
--   rather than relying upon a custom encoding/decoding
--   specification.
--
--   Due to the explicit endianness specifications, all serialisation
--   libraries should use the same encoding for values of this type.
data BenchWord
    = BenchWord (LittleEndian Word64) (BigEndian Word64)
                (LittleEndian Word32) (BigEndian Word32)
                (LittleEndian Word16) (BigEndian Word16)
                              Word8              Word8
  deriving (Eq, Show, Generic, NFData, Theseus, B.Binary, C.Serialize)

zeroBenchWord :: BenchWord
zeroBenchWord = BenchWord 0 0 0 0 0 0 0 0

benchWordSize :: Int
benchWordSize = sizeOfValue zeroBenchWord

genBenchData :: Int -> ([BenchWord], [ByteString])
genBenchData count =
    unzip $ map (\ x -> (mkBenchWord x, mkBenchBS x)) [ 1 .. count ]
  where
    mkBenchWord i =
        BenchWord (fromIntegral $ 137 * i) (fromIntegral $ 231 * i)
                    (fromIntegral $ 51 * i) (fromIntegral $ 71 * i)
                    (fromIntegral $ 3 * i) (fromIntegral $ 5 * i)
                    (fromIntegral i) (fromIntegral $ i + 1)

    -- Number (30 in this case) needs to be the same as the packed size.
    mkBenchBS i = CBS.replicate benchWordSize (chr i)
