{-# LANGUAGE DefaultSignatures, FlexibleContexts, TupleSections, TypeOperators
             #-}
{- |
   Module      : Data.Theseus.Values
   Description : How to store individual values
   Copyright   : Ivan Lazar Miljenovic, Patryk Zadarnowski
   License     : MIT
   Maintainer  : Ivan.Miljenovic@gmail.com



 -}
module Data.Theseus.Values where

import           Control.Arrow            (first)
import           Data.Bool                (bool)
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as B
import qualified Data.ByteString.Internal as B
import           Data.Int
import           Data.Word
import           Foreign.ForeignPtr
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           Foreign.Storable
import           GHC.Generics

--------------------------------------------------------------------------------

-- | (offset + required length) (i.e. offset of next argument)
type LenCheck = Int -> IO ()

class Theseus a where
  sizeOfValue :: a -> Int
  default sizeOfValue :: (Generic a, GTheseus (Rep a)) => a -> Int
  sizeOfValue = gSizeOfValue . from
  {-# INLINE sizeOfValue #-}

  -- | Int argument is offset; returns length
  decodeValue :: LenCheck -> ByteString -> Ptr x -> Int -> IO (a, Int)
  default decodeValue :: (Generic a, GTheseus (Rep a))
                         => LenCheck -> ByteString -> Ptr x -> Int -> IO (a, Int)
  decodeValue lc b p o = first to <$> gDecodeValue lc b p o
  {-# INLINE decodeValue #-}

  -- | Takes offset
  encodeValue :: Ptr x -> Int -> a -> IO ()
  default encodeValue :: (Generic a, GTheseus (Rep a)) => Ptr x -> Int -> a -> IO ()
  encodeValue p o = gEncodeValue p o . from
  {-# INLINE encodeValue #-}

instance Theseus Int8 where
  sizeOfValue = sizeOf
  {-# INLINE sizeOfValue #-}

  decodeValue lc _ p o = do lc olen
                            (,olen) <$> peekByteOff p o
    where
      olen = o + sizeOf (0::Int8)
  {-# INLINE decodeValue #-}

  encodeValue = pokeByteOff
  {-# INLINE encodeValue #-}

instance Theseus Int16 where
  sizeOfValue = sizeOf
  {-# INLINE sizeOfValue #-}

  decodeValue lc _ p o = do lc olen
                            (,olen) <$> peekByteOff p o
    where
      olen = o + sizeOf (0::Int16)
  {-# INLINE decodeValue #-}

  encodeValue = pokeByteOff
  {-# INLINE encodeValue #-}

instance Theseus Int32 where
  sizeOfValue = sizeOf
  {-# INLINE sizeOfValue #-}

  decodeValue lc _ p o = do lc olen
                            (,olen) <$> peekByteOff p o
    where
      olen = o + sizeOf (0::Int32)
  {-# INLINE decodeValue #-}

  encodeValue = pokeByteOff
  {-# INLINE encodeValue #-}

instance Theseus Int64 where
  sizeOfValue = sizeOf
  {-# INLINE sizeOfValue #-}

  decodeValue lc _ p o = do lc olen
                            (,olen) <$> peekByteOff p o
    where
      olen = o + sizeOf (0::Int64)
  {-# INLINE decodeValue #-}

  encodeValue = pokeByteOff
  {-# INLINE encodeValue #-}

instance Theseus Word8 where
  sizeOfValue = sizeOf
  {-# INLINE sizeOfValue #-}

  decodeValue lc _ p o = do lc olen
                            (,olen) <$> peekByteOff p o
    where
      olen = o + sizeOf (0::Word8)
  {-# INLINE decodeValue #-}

  encodeValue = pokeByteOff
  {-# INLINE encodeValue #-}

instance Theseus Word16 where
  sizeOfValue = sizeOf
  {-# INLINE sizeOfValue #-}

  decodeValue lc _ p o = do lc olen
                            (,olen) <$> peekByteOff p o
    where
      olen = o + sizeOf (0::Word16)
  {-# INLINE decodeValue #-}

  encodeValue = pokeByteOff
  {-# INLINE encodeValue #-}

instance Theseus Word32 where
  sizeOfValue = sizeOf
  {-# INLINE sizeOfValue #-}

  decodeValue lc _ p o = do lc olen
                            (,olen) <$> peekByteOff p o
    where
      olen = o + sizeOf (0::Word32)
  {-# INLINE decodeValue #-}

  encodeValue = pokeByteOff
  {-# INLINE encodeValue #-}

instance Theseus Word64 where
  sizeOfValue = sizeOf
  {-# INLINE sizeOfValue #-}

  decodeValue lc _ p o = do lc olen
                            (,olen) <$> peekByteOff p o
    where
      olen = o + sizeOf (0::Word64)
  {-# INLINE decodeValue #-}

  encodeValue = pokeByteOff
  {-# INLINE encodeValue #-}

instance Theseus ByteString where
  sizeOfValue = sizeOfByteString
  {-# INLINE sizeOfValue #-}

  decodeValue lc b p o = do let odata = o + sizeOfLength
                            lc odata
                            len <- peekLengthOff p o
                            -- Assume that if we have the length
                            -- set, then we have the actual
                            -- ByteString here.
                            let olen = odata + len
                            pure (substr odata len b, olen)
  {-# INLINE decodeValue #-}

  encodeValue = pokeByteStringOff
  {-# INLINE encodeValue #-}

instance Theseus () where
  sizeOfValue = sizeOf
  {-# INLINE sizeOfValue #-}

  decodeValue _ _ _ o = return ((),o)
  {-# INLINE decodeValue #-}

  encodeValue _ _ _ = return ()
  {-# INLINE encodeValue #-}

instance (Theseus a, Theseus b) => Theseus (a,b)
instance (Theseus a, Theseus b, Theseus c) => Theseus (a,b,c)
instance (Theseus a, Theseus b, Theseus c, Theseus d) => Theseus (a,b,c,d)

falseWord8 :: Word8
falseWord8 = 0

instance Theseus Bool where
  sizeOfValue = const (sizeOfValue falseWord8)
  {-# INLINE sizeOfValue #-}

  decodeValue lc b p o = first (/= falseWord8) <$> decodeValue lc b p o
  {-# INLINE decodeValue #-}

  encodeValue p o = encodeValue p o . bool falseWord8 1
  {-# INLINE encodeValue #-}

--------------------------------------------------------------------------------

class GTheseus f where
  gSizeOfValue :: f a -> Int
  gDecodeValue :: LenCheck -> ByteString -> Ptr x -> Int -> IO (f a, Int)
  gEncodeValue :: Ptr x -> Int -> f a -> IO ()

-- Product type
instance (GTheseus f, GTheseus g) => GTheseus (f :*: g) where
  gSizeOfValue (a :*: b) = gSizeOfValue a + gSizeOfValue b
  {-# INLINE gSizeOfValue #-}

  gDecodeValue lc b p o = do (a,o') <- gDecodeValue lc b p o
                             first (a :*:) <$> gDecodeValue lc b p o'
  {-# INLINE gDecodeValue #-}

  gEncodeValue p o (a :*: b) = gEncodeValue p o a
                               *> gEncodeValue p (o + gSizeOfValue a) b
  {-# INLINE gEncodeValue #-}

-- Equivalent to a single value.
instance (Theseus c) => GTheseus (K1 i c) where
  gSizeOfValue = sizeOfValue . unK1
  {-# INLINE gSizeOfValue #-}

  gDecodeValue lc b p o = first K1 <$> decodeValue lc b p o
  {-# INLINE gDecodeValue #-}

  gEncodeValue p o = encodeValue p o . unK1
  {-# INLINE gEncodeValue #-}

-- Meta-information
instance (GTheseus f) => GTheseus (M1 i t f) where
  gSizeOfValue = gSizeOfValue . unM1
  {-# INLINE gSizeOfValue #-}

  gDecodeValue lc b p o = first M1 <$> gDecodeValue lc b p o
  {-# INLINE gDecodeValue #-}

  gEncodeValue p o = gEncodeValue p o . unM1
  {-# INLINE gEncodeValue #-}

--------------------------------------------------------------------------------

-- | Number of bytes reserved for storing the length of a ByteString
sizeOfLength :: Int
sizeOfLength = sizeOf (0::Word32)

-- Off is short for Offset

peekLengthOff :: Ptr a -> Int -> IO Int
peekLengthOff p o = fromIntegral <$> (peekByteOff p o :: IO Word32)

pokeLengthOff :: Ptr a -> Int -> Int -> IO ()
pokeLengthOff p o x = pokeByteOff p o (fromIntegral x :: Word32)

sizeOfByteString :: ByteString -> Int
sizeOfByteString s = sizeOfLength + B.length s

pokeByteStringOff :: Ptr a -> Int -> ByteString -> IO ()
pokeByteStringOff dptr doff s = do
  let (fp, o, n) = B.toForeignPtr s
  pokeLengthOff dptr doff n
  withForeignPtr fp $ \p ->
    moveArray (plusPtr dptr (doff + sizeOfLength) :: Ptr Word8) (plusPtr p o) n

-- | Takes offset and size
substr :: Int -> Int -> ByteString -> ByteString
substr off sz s
  | sz == 0   = B.empty
  | otherwise = B.take sz . B.drop off $ s
