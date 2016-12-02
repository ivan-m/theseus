{-# LANGUAGE CPP, DefaultSignatures, FlexibleContexts, FlexibleInstances,
             TupleSections, TypeOperators #-}

{- |
   Module      : Data.Theseus.Values
   Description : How to store individual values
   Copyright   : Ivan Lazar Miljenovic, Patryk Zadarnowski
   License     : MIT
   Maintainer  : Ivan.Miljenovic@gmail.com

   You almost definitely do /not/ want to import this.

   It's only going to really be useful if you need/want to manually
   construct a 'Theseus' instance (and that only makes sense for
   base-level types).

 -}
module Data.Theseus.Values where

import           Control.Arrow            (first)
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as B
import qualified Data.ByteString.Internal as B
import           Data.Storable.Endian
import           Data.Word
import           Foreign.ForeignPtr
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           Foreign.Storable
import           GHC.Generics

-- Just for instances
import           Control.Applicative   (ZipList)
import           Data.Complex          (Complex)
import qualified Data.Functor.Compose  as F
import qualified Data.Functor.Const    as F
import qualified Data.Functor.Identity as F
import qualified Data.Functor.Product  as F
import qualified Data.Functor.Sum      as F
import           Data.Int
import           Data.List.NonEmpty    (NonEmpty)
import           Data.Monoid           (All, Alt, Any, Dual, First, Last,
                                        Product, Sum)
import           Data.Proxy            (Proxy)
import qualified Data.Semigroup        as S

--------------------------------------------------------------------------------

-- | (offset + required length) (i.e. offset of next argument)
type LenCheck = Int -> IO ()

-- | How to serialise and deserialise an individual value.
--
--   Note that some types (such as @Int@) do /not/ have instances, as
--   their serialisation is platform-dependent.
class Theseus a where

  -- | The encoded size of a value in bytes.  This is needed for
  --   encoding product types.
  sizeOfValue :: a -> Int
  default sizeOfValue :: (Generic a, GTheseus (Rep a)) => a -> Int
  sizeOfValue = gSizeOfValue . from
  {-# INLINE sizeOfValue #-}

  -- | Decode a value starting at the specified offset.  Returns the
  --   decoded value and the offset for the next value.
  decodeValue :: LenCheck -> ByteString -> Ptr x -> Int -> IO (a, Int)
  default decodeValue :: (Generic a, GTheseus (Rep a))
                         => LenCheck -> ByteString -> Ptr x -> Int -> IO (a, Int)
  decodeValue lc b p o = first to <$> gDecodeValue lc b p o
  {-# INLINE decodeValue #-}

  -- | Encode a value, starting at the specified offset.
  encodeValue :: Ptr x -> Int -> a -> IO ()
  default encodeValue :: (Generic a, GTheseus (Rep a)) => Ptr x -> Int -> a -> IO ()
  encodeValue p o = gEncodeValue p o . from
  {-# INLINE encodeValue #-}

#define THESEUS(T)                                          \
instance Theseus (T) where {                                \
  sizeOfValue = sizeOf;                                     \
  {-# INLINE sizeOfValue #-};                               \
                                                            \
  decodeValue lc _ p o = do { lc olen ;                     \
                              (,olen) <$> peekByteOff p o } \
    where {                                                 \
      olen = o + sizeOf (undefined::(T)) };                 \
  {-# INLINE decodeValue #-} ;                              \
                                                            \
  encodeValue = pokeByteOff ;                               \
  {-# INLINE encodeValue #-}                                \
}

#define THESEUS_E(T)     \
THESEUS(T);              \
THESEUS(LittleEndian T); \
THESEUS(BigEndian T);    \

THESEUS(Int8)

THESEUS_E(Int16)

THESEUS_E(Int32)

THESEUS_E(Int64)

THESEUS(Word8)

THESEUS_E(Word16)

THESEUS_E(Word32)

THESEUS_E(Word64)

THESEUS_E(Double)

THESEUS_E(Float)

THESEUS(Char)

THESEUS(Ptr a)

THESEUS(FunPtr a)

THESEUS(WordPtr)

THESEUS(IntPtr)

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
                            lc olen
                            pure (substr odata len b, olen)
  {-# INLINE decodeValue #-}

  encodeValue = pokeByteStringOff
  {-# INLINE encodeValue #-}

instance Theseus ()
instance (Theseus a, Theseus b) => Theseus (a,b)
instance (Theseus a, Theseus b, Theseus c) => Theseus (a,b,c)
instance (Theseus a, Theseus b, Theseus c, Theseus d) => Theseus (a,b,c,d)

sizeWord8 :: Int
sizeWord8 = sizeOfValue (0::Word8)

instance Theseus Bool

instance Theseus Ordering

instance (Theseus a, Theseus b) => Theseus (Either a b)

instance (Theseus a) => Theseus (Maybe a)

instance (Theseus a) => Theseus [a]

instance (Theseus a) => Theseus (ZipList a)

instance (Theseus a) => Theseus (NonEmpty a)

instance Theseus All

instance (Theseus (f a)) => Theseus (Alt f a)

instance Theseus Any

instance (Theseus a) => Theseus (Dual a)

instance (Theseus a) => Theseus (First a)

instance (Theseus a) => Theseus (Last a)

instance (Theseus a) => Theseus (Product a)

instance (Theseus a) => Theseus (Sum a)

instance (Theseus a) => Theseus (Complex a)

instance (Theseus a) => Theseus (F.Identity a)

instance (Theseus a) => Theseus (F.Const a b)

instance (Theseus (f a), Theseus (g a)) => Theseus (F.Sum f g a)

instance (Theseus (f a), Theseus (g a)) => Theseus (F.Product f g a)

instance (Theseus (f (g a))) => Theseus (F.Compose f g a)

instance (Theseus a) => Theseus (S.Min a)

instance (Theseus a) => Theseus (S.Max a)

instance (Theseus a) => Theseus (S.First a)

instance (Theseus a) => Theseus (S.Last a)

instance (Theseus m) => Theseus (S.WrappedMonoid m)

instance (Theseus a) => Theseus (S.Option a)

instance (Theseus a, Theseus b) => Theseus (S.Arg a b)

instance Theseus (Proxy a)

--------------------------------------------------------------------------------

-- | These take an extra parameter when encoding\/decoding sum types
--   (i.e. more than one possible constructor).  It is assumed that no
--   type will have more than @256@ constructors.
class GTheseus f where
  gSizeOfValue :: f a -> Int
  gDecodeValue' :: Word8 -> LenCheck -> ByteString -> Ptr x -> Int -> IO (f a, Int)
  gEncodeValue' :: Word8 -> Ptr x -> Int -> f a -> IO ()

gDecodeValue :: (GTheseus f) => LenCheck -> ByteString -> Ptr x -> Int -> IO (f a, Int)
gDecodeValue = gDecodeValue' 0
{-# INLINE gDecodeValue #-}

gEncodeValue :: (GTheseus f) => Ptr x -> Int -> f a -> IO ()
gEncodeValue = gEncodeValue' 0
{-# INLINE gEncodeValue #-}

-- Product type
instance (GTheseus f, GTheseus g) => GTheseus (f :*: g) where
  gSizeOfValue (a :*: b) = gSizeOfValue a + gSizeOfValue b
  {-# INLINE gSizeOfValue #-}

  gDecodeValue' _ lc b p o = do (a,o') <- gDecodeValue lc b p o
                                first (a :*:) <$> gDecodeValue lc b p o'
  {-# INLINE gDecodeValue' #-}

  gEncodeValue' _ p o (a :*: b) = gEncodeValue p o a
                                  *> gEncodeValue p (o + gSizeOfValue a) b
  {-# INLINE gEncodeValue' #-}

-- Nested sum type
instance (GTheseus f, GTheseus g, GTheseus h) => GTheseus (f :+: (g :+: h)) where
  gSizeOfValue = go
    where
      go (L1 a) = gSizeOfValue a + sizeWord8
      go (R1 b) = gSizeOfValue b -- Will have the size of the tag in here
  {-# INLINE gSizeOfValue #-}

  gDecodeValue' c lc b p o = do (c',o') <- decodeValue lc b p o
                                if (c == c')
                                   then -- This is the correct constructor
                                        first L1 <$> gDecodeValue lc b p o'
                                   else first R1 <$> gDecodeValue' (c+1) lc b p o
                                        -- Using original offset!
  {-# INLINE gDecodeValue' #-}

  gEncodeValue' c p o (L1 a) = encodeValue p o c *> gEncodeValue p (o + sizeWord8) a
  gEncodeValue' c p o (R1 b) = let c1 = c + 1
                               in c1 `seq` gEncodeValue' c1 p o b
  {-# INLINE gEncodeValue' #-}

-- Nested sum type with metadata
instance (GTheseus f, GTheseus g, GTheseus h) => GTheseus (f :+: M1 i t (g :+: h)) where
  gSizeOfValue = (sizeWord8 +) . go
    where
      go (L1 a) = gSizeOfValue a
      go (R1 b) = gSizeOfValue b
  {-# INLINE gSizeOfValue #-}

  gDecodeValue' c lc b p o = do (c',o') <- decodeValue lc b p o
                                if (c == c')
                                   then -- This is the correct constructor
                                        first L1 <$> gDecodeValue lc b p o'
                                   else first R1 <$> gDecodeValue' (c+1) lc b p o
                                        -- Using original offset!
  {-# INLINE gDecodeValue' #-}

  gEncodeValue' c p o (L1 a) = encodeValue p o c *> gEncodeValue p (o + sizeWord8) a
  gEncodeValue' c p o (R1 b) = let c1 = c + 1
                               in c1 `seq` gEncodeValue' c1 p o b
  {-# INLINE gEncodeValue' #-}

instance {-# OVERLAPPABLE #-} (GTheseus f, GTheseus g) => GTheseus (f :+: g) where
  gSizeOfValue = (sizeWord8 +) . go
    where
      go (L1 a) = gSizeOfValue a
      go (R1 b) = gSizeOfValue b
  {-# INLINE gSizeOfValue #-}

  gDecodeValue' c lc b p o = do (c',o') <- decodeValue lc b p o
                                if (c == c')
                                   then -- This is the correct constructor
                                        first L1 <$> gDecodeValue lc b p o'
                                   else first R1 <$> gDecodeValue lc b p o'
                                        -- Last constructor, who cares what the code is
  {-# INLINE gDecodeValue' #-}

  gEncodeValue' c p o (L1 a) = encodeValue p o c     *> gEncodeValue p (o + sizeWord8) a
  gEncodeValue' c p o (R1 b) = encodeValue p o (c+1) *> gEncodeValue p (o + sizeWord8) b
  {-# INLINE gEncodeValue' #-}

-- Equivalent to a single value.
instance (Theseus c) => GTheseus (K1 i c) where
  gSizeOfValue = sizeOfValue . unK1
  {-# INLINE gSizeOfValue #-}

  gDecodeValue' _ lc b p o = first K1 <$> decodeValue lc b p o
  {-# INLINE gDecodeValue' #-}

  gEncodeValue' _ p o = encodeValue p o . unK1
  {-# INLINE gEncodeValue' #-}

-- Meta-information
instance (GTheseus f) => GTheseus (M1 i t f) where
  gSizeOfValue = gSizeOfValue . unM1
  {-# INLINE gSizeOfValue #-}

  -- Need to pass through constructor depth for sum-types

  gDecodeValue' c lc b p o = first M1 <$> gDecodeValue' c lc b p o
  {-# INLINE gDecodeValue' #-}

  gEncodeValue' c p o = gEncodeValue' c p o . unM1
  {-# INLINE gEncodeValue' #-}

-- Constructors without arguments
instance GTheseus U1 where
  gSizeOfValue = const 0
  {-# INLINE gSizeOfValue #-}

  gDecodeValue' _ _ _ _ o = return (U1, o)
  {-# INLINE gDecodeValue' #-}

  gEncodeValue' _ _ _ _ = return ()
  {-# INLINE gEncodeValue' #-}

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
