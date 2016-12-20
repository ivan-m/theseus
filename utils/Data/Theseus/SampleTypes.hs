{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}

{- |
   Module      : Data.Theseus.SampleTypes
   Description : Complicated types for tests and benchmarks
   Copyright   : Ivan Lazar Miljenovic, Patryk Zadarnowski
   License     : MIT
   Maintainer  : Ivan.Miljenovic@gmail.com



 -}
module Data.Theseus.SampleTypes where

import Data.Theseus

import Data.ByteString      (ByteString)
import Data.Storable.Endian (BigEndian(..), LittleEndian(..))
import Data.Word
import GHC.Generics

--------------------------------------------------------------------------------

data InnerStructure a = ISOne a ((), Ordering, Double)
                      | ISTwo
                      | ISThree { _three1 :: ByteString
                                , _three2 :: (Word8, BigEndian Word16, LittleEndian Word32, Word64)
                                , _three3 :: String
                                }
  deriving (Eq, Show, Read, Generic, Theseus)

data OuterStructure = OS Bool
                         (Either Char ())
                         (InnerStructure Word8)
                         (Maybe ByteString)
  deriving (Eq, Show, Read, Generic, Theseus)
