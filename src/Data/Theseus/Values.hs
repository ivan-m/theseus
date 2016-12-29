{-# LANGUAGE CPP, DefaultSignatures, FlexibleContexts, FlexibleInstances,
             ScopedTypeVariables, TupleSections, TypeOperators #-}

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
import qualified Data.ByteString.Lazy  as LB
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
import           Data.Proxy            (Proxy(..))
import qualified Data.Semigroup        as S

--------------------------------------------------------------------------------

type Offset = Int

-- | (offset + required length) (i.e. offset of next argument)
type LenCheck = Offset -> IO ()

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
  decodeValue :: LenCheck -> ByteString -> Ptr x -> Offset -> IO (a, Offset)
  default decodeValue :: (Generic a, GTheseus (Rep a))
                         => LenCheck -> ByteString -> Ptr x -> Offset -> IO (a, Offset)
  decodeValue lc b p o = first to <$> gDecodeValue lc b p o
  {-# INLINE decodeValue #-}

  -- | Encode a value, starting at the specified offset.
  encodeValue :: Ptr x -> Offset -> a -> IO ()
  default encodeValue :: (Generic a, GTheseus (Rep a)) => Ptr x -> Offset -> a -> IO ()
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

instance Theseus LB.ByteString where
  sizeOfValue = sizeOfValue . LB.toChunks
  {-# INLINE sizeOfValue #-}

  decodeValue lc b p o = first LB.fromChunks <$> decodeValue lc b p o
  {-# INLINE decodeValue #-}

  encodeValue ptr o = encodeValue ptr o . LB.toChunks
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

-- | These take an extra two parameters when encoding\/decoding to be
--   able to handle sum types (i.e. more than one possible
--   constructor).  It is assumed that no type will have more than
--   @256@ constructors.
class GTheseus f where
  gSizeOfValue :: f a -> Int

  gDecodeValue :: LenCheck -> ByteString -> Ptr x -> Offset -> IO (f a, Offset)

  gEncodeValue :: Ptr x -> Offset -> f a -> IO ()

-- Product type
instance (GTheseus f, GTheseus g) => GTheseus (f :*: g) where
  gSizeOfValue (a :*: b) = gSizeOfValue a + gSizeOfValue b
  {-# INLINE gSizeOfValue #-}

  gDecodeValue lc b ptr o = do (a,o') <- gDecodeValue lc b ptr o
                               first (a :*:) <$> gDecodeValue lc b ptr o'
  {-# INLINE gDecodeValue #-}

  gEncodeValue ptr o (a :*: b) = gEncodeValue ptr o a
                                 *> gEncodeValue ptr (o + gSizeOfValue a) b
  {-# INLINE gEncodeValue #-}

instance (GConstructors f, GConstructors g) => GTheseus (f :+: g) where
  gSizeOfValue = gConstructSize
  {-# INLINE gSizeOfValue #-}

  gDecodeValue lc b ptr o = do (cw,o') <- decodeValue lc b ptr o
                               gConstructorDecode cw lc b ptr o'
  {-# INLINE gDecodeValue #-}

  gEncodeValue = gConstructorEncode
  {-# INLINE gEncodeValue #-}

-- Equivalent to a single value.
instance (Theseus c) => GTheseus (K1 i c) where
  gSizeOfValue = sizeOfValue . unK1
  {-# INLINE gSizeOfValue #-}

  gDecodeValue lc b ptr o = first K1 <$> decodeValue lc b ptr o
  {-# INLINE gDecodeValue #-}

  gEncodeValue ptr o = encodeValue ptr o . unK1
  {-# INLINE gEncodeValue #-}

-- Meta-information
instance (GTheseus f) => GTheseus (M1 i t f) where
  gSizeOfValue = gSizeOfValue . unM1
  {-# INLINE gSizeOfValue #-}

  gDecodeValue lc b ptr o = first M1 <$> gDecodeValue lc b ptr o
  {-# INLINE gDecodeValue #-}

  gEncodeValue ptr o = gEncodeValue ptr o . unM1
  {-# INLINE gEncodeValue #-}

-- Constructors without arguments
instance GTheseus U1 where
  gSizeOfValue = const 0
  {-# INLINE gSizeOfValue #-}

  gDecodeValue _ _ _ o = return (U1, o)
  {-# INLINE gDecodeValue #-}

  gEncodeValue _ _ _ = return ()
  {-# INLINE gEncodeValue #-}

--------------------------------------------------------------------------------

type NumConstruct = Word8

-- | Handle multiple constructors
class (GTheseus f) => GConstructors f where
  gConstructSize :: f a -> Int

  gNumConstruct :: Proxy f -> NumConstruct

  -- | First NumConstruct is for the constructor we're currently up
  --   to; second is for the one we're searching for.
  gConstructorDecode' :: Proxy f -> NumConstruct -> NumConstruct
                         -> LenCheck -> ByteString -> Ptr x -> Offset -> IO (f a, Offset)

  gConstructorEncode' :: Proxy f -> NumConstruct -> Ptr x -> Offset -> f a -> IO ()

gConstructorDecode :: forall f a x. (GConstructors f) => NumConstruct
                      -> LenCheck -> ByteString -> Ptr x -> Offset -> IO (f a, Offset)
gConstructorDecode = gConstructorDecode' (Proxy :: Proxy f) 0
{-# INLINE gConstructorDecode #-}

gConstructorEncode :: forall f a x. (GConstructors f) => Ptr x -> Offset -> f a -> IO ()
gConstructorEncode = gConstructorEncode' (Proxy :: Proxy f) 0
{-# INLINE gConstructorEncode #-}

instance (GTheseus f) => GConstructors (M1 C t f) where
  gConstructSize = (sizeWord8 +) . gSizeOfValue
  {-# INLINE gConstructSize #-}

  gNumConstruct _ = 1
  {-# INLINE gNumConstruct #-}

  gConstructorDecode' _ _ _ = gDecodeValue
  {-# INLINE gConstructorDecode' #-}

  gConstructorEncode' _ c ptr o ca = encodeValue ptr o c
                                     *> gEncodeValue ptr (o + sizeWord8) (unM1 ca)
  {-# INLINE gConstructorEncode' #-}

instance (GConstructors f, GConstructors g) => GConstructors (f :+: g) where
  gConstructSize (L1 a) = gConstructSize a
  gConstructSize (R1 b) = gConstructSize b
  {-# INLINE gConstructSize #-}

  gNumConstruct p = gNumConstruct (leftSum p) + gNumConstruct (rightSum p)
  {-# INLINE gNumConstruct #-}

  gConstructorDecode' pr c cw lc b ptr o
    = if cw < cShift
        then first L1 <$> gConstructorDecode' prLeft        c      cw lc b ptr o
        else first R1 <$> gConstructorDecode' (rightSum pr) cShift cw lc b ptr o
    where
      prLeft = leftSum pr
      cLeft = gNumConstruct prLeft
      cShift = c + cLeft
  {-# INLINE gConstructorDecode' #-}

  gConstructorEncode' pr c ptr o (L1 a) = gConstructorEncode' (leftSum pr) c ptr o a
  gConstructorEncode' pr c ptr o (R1 b) = let cr = c + gNumConstruct (leftSum pr)
                                          in gConstructorEncode' (rightSum pr) cr ptr o b
  {-# INLINE gConstructorEncode' #-}

unMeta :: Proxy (M1 i t f) -> Proxy f
unMeta _ = Proxy
{-# INLINE unMeta #-}

leftSum :: Proxy (f :+: g) -> Proxy f
leftSum _ = Proxy
{-# INLINE leftSum #-}

rightSum :: Proxy (f :+: g) -> Proxy g
rightSum _ = Proxy
{-# INLINE rightSum #-}

--------------------------------------------------------------------------------

-- | Number of bytes reserved for storing the length of a ByteString
sizeOfLength :: Int
sizeOfLength = sizeOf (0::Word32)

-- Off is short for Offset

peekLengthOff :: Ptr a -> Offset -> IO Offset
peekLengthOff p o = fromIntegral <$> (peekByteOff p o :: IO Word32)

pokeLengthOff :: Ptr a -> Offset -> Offset -> IO ()
pokeLengthOff p o x = pokeByteOff p o (fromIntegral x :: Word32)

sizeOfByteString :: ByteString -> Int
sizeOfByteString s = sizeOfLength + B.length s

pokeByteStringOff :: Ptr a -> Offset -> ByteString -> IO ()
pokeByteStringOff dptr doff s = do
  let (fp, o, n) = B.toForeignPtr s
  pokeLengthOff dptr doff n
  withForeignPtr fp $ \p ->
    moveArray (plusPtr dptr (doff + sizeOfLength) :: Ptr Word8) (plusPtr p o) n

-- | Takes offset and size
substr :: Offset -> Offset -> ByteString -> ByteString
substr off sz s
  | sz == 0   = B.empty
  | otherwise = B.take sz . B.drop off $ s
