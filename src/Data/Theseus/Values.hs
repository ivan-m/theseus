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

  -- | Number of constructors available for this type.  This will
  --   usually be @1@.
  numConstructors :: Proxy a -> Word8
  default numConstructors :: (Generic a, GTheseus (Rep a)) => Proxy a -> Word8
  numConstructors = gNumConstructors . fmap from

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
  numConstructors = const 1;                                \
  {-# INLINE numConstructors #-};                           \
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

  numConstructors = const 1
  {-# INLINE numConstructors #-}

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

-- | These take an extra two parameters when encoding\/decoding to be
--   able to handle sum types (i.e. more than one possible
--   constructor).  It is assumed that no type will have more than
--   @256@ constructors.
class GTheseus f where
  gSizeOfValue :: f a -> Int

  gNumConstructors :: Proxy (f a) -> Word8
  gNumConstructors = const 1
  {-# INLINE gNumConstructors #-}

  gDecodeValue' :: Proxy (f a) -> Word8 -> LenCheck -> ByteString -> Ptr x -> Int -> IO (f a, Int)

  gEncodeValue' :: Proxy (f a) -> Word8 -> Ptr x -> Int -> f a -> IO ()

gDecodeValue :: forall f a x. (GTheseus f) => LenCheck -> ByteString -> Ptr x -> Int -> IO (f a, Int)
gDecodeValue = gDecodeValue' (Proxy :: Proxy (f a)) 0
{-# INLINE gDecodeValue #-}

gEncodeValue :: forall f a x. (GTheseus f) => Ptr x -> Int -> f a -> IO ()
gEncodeValue = gEncodeValue' (Proxy :: Proxy (f a)) 0
{-# INLINE gEncodeValue #-}

-- Product type
instance (GTheseus f, GTheseus g) => GTheseus (f :*: g) where
  gSizeOfValue (a :*: b) = gSizeOfValue a + gSizeOfValue b
  {-# INLINE gSizeOfValue #-}

  gDecodeValue' _ _ lc b ptr o = do (a,o') <- gDecodeValue lc b ptr o
                                    first (a :*:) <$> gDecodeValue lc b ptr o'
  {-# INLINE gDecodeValue' #-}

  gEncodeValue' _ _ ptr o (a :*: b) = gEncodeValue ptr o a
                                      *> gEncodeValue ptr (o + gSizeOfValue a) b
  {-# INLINE gEncodeValue' #-}

leftSum :: Proxy ((f :+: g) a) -> Proxy (f a)
leftSum _ = Proxy
{-# INLINE leftSum #-}

rightSum :: Proxy ((f :+: g) a) -> Proxy (g a)
rightSum _ = Proxy
{-# INLINE rightSum #-}

-- | Bi-Nested sum type
instance {-# Overlapping #-} (GTheseus f, GTheseus g, GTheseus h, GTheseus i) => GTheseus ((f :+: g) :+: (h :+: i)) where
  gSizeOfValue = go
    where
      -- Will have the size of the tag in both sides already
      go (L1 a) = gSizeOfValue a
      go (R1 b) = gSizeOfValue b
  {-# INLINE gSizeOfValue #-}

  -- We do /not/ want to just add constructors for f, g, h and i
  -- individuall, in case we have a sum type containing a sum type as
  -- a parameter.
  --
  -- There's probably a better way of expressing this actual constraint.
  gNumConstructors p =   gNumConstructors (leftSum p)
                       + gNumConstructors (rightSum p)
  {-# INLINE gNumConstructors #-}

  gDecodeValue' pr c lc b ptr o = do (c',_) <- decodeValue lc b ptr o
                                     -- We will have to read this again, so don't use the offset
                                     if (c' < c + cLeft) -- 0-indexed, so don't use <=
                                        then -- Left branch
                                             first L1 <$> gDecodeValue' prLeft c lc b ptr o
                                        else let cr = c + cLeft
                                             in cr `seq` first R1 <$> gDecodeValue' (rightSum pr) cr lc b ptr o
    where
      prLeft = leftSum pr
      cLeft = gNumConstructors prLeft
  {-# INLINE gDecodeValue' #-}

  gEncodeValue' pr c ptr o (L1 a) = gEncodeValue' (leftSum pr) c ptr o a
  gEncodeValue' pr c ptr o (R1 b) = let cr = c + gNumConstructors (leftSum pr)
                                    in cr `seq` gEncodeValue' (rightSum pr) cr ptr o b
  {-# INLINE gEncodeValue' #-}

-- | Left-Nested sum type
instance {-# Overlappable #-} (GTheseus f, GTheseus g, GTheseus h) => GTheseus ((f :+: g) :+: h) where
  gSizeOfValue = go
    where
      go (L1 a) = gSizeOfValue a -- Will have the size of the tag in here
      go (R1 b) = gSizeOfValue b + sizeWord8
  {-# INLINE gSizeOfValue #-}

  gNumConstructors p = gNumConstructors (leftSum p) + 1
  {-# INLINE gNumConstructors #-}

  gDecodeValue' pr c lc b ptr o = do (c',o') <- decodeValue lc b ptr o
                                     if (c' < c + gNumConstructors prLeft)
                                        then -- This is the correct constructor
                                             first L1 <$> gDecodeValue' prLeft c lc b ptr o
                                        else first R1 <$> gDecodeValue lc b ptr o'
                                             -- This must be it.
    where
      prLeft = leftSum pr
  {-# INLINE gDecodeValue' #-}

  gEncodeValue' pr c ptr o (L1 a) = gEncodeValue' (leftSum pr) c ptr o a
  gEncodeValue' pr c ptr o (R1 b) = let cr = c + gNumConstructors (leftSum pr)
                                    in encodeValue ptr o cr *> gEncodeValue ptr (o + sizeWord8) b
  {-# INLINE gEncodeValue' #-}

-- | Right-Nested sum type
instance {-# Overlappable #-} (GTheseus f, GTheseus g, GTheseus h) => GTheseus (f :+: (g :+: h)) where
  gSizeOfValue = go
    where
      go (L1 a) = gSizeOfValue a + sizeWord8
      go (R1 b) = gSizeOfValue b -- Will have the size of the tag in here
  {-# INLINE gSizeOfValue #-}

  gNumConstructors p = 1 + gNumConstructors (rightSum p)
  {-# INLINE gNumConstructors #-}

  gDecodeValue' pr c lc b ptr o = do (c',o') <- decodeValue lc b ptr o
                                     if (c == c')
                                        then -- This is the correct constructor
                                             first L1 <$> gDecodeValue lc b ptr o'
                                             -- left has 1 constructor
                                        else let cr = c + 1
                                              in cr `seq` first R1 <$> gDecodeValue' (rightSum pr) cr lc b ptr o
                                             -- Using original offset!
  {-# INLINE gDecodeValue' #-}

  gEncodeValue' _  c ptr o (L1 a) = encodeValue ptr o c *> gEncodeValue ptr (o + sizeWord8) a
  gEncodeValue' pr c ptr o (R1 b) = let cr = c + 1 -- Left has 1 constructor
                                    in cr `seq` gEncodeValue' (rightSum pr) cr ptr o b
  {-# INLINE gEncodeValue' #-}

instance {-# OVERLAPPABLE #-} (GTheseus f, GTheseus g) => GTheseus (f :+: g) where
  gSizeOfValue = (sizeWord8 +) . go
    where
      go (L1 a) = gSizeOfValue a
      go (R1 b) = gSizeOfValue b
  {-# INLINE gSizeOfValue #-}

  gNumConstructors _ = 2
  {-# INLINE gNumConstructors #-}

  gDecodeValue' _ c lc b ptr o = do (c',o') <- decodeValue lc b ptr o
                                    if (c == c')
                                       then -- This is the correct constructor
                                            first L1 <$> gDecodeValue lc b ptr o'
                                       else first R1 <$> gDecodeValue lc b ptr o'
                                            -- Last constructor, who cares what the code is
  {-# INLINE gDecodeValue' #-}

  gEncodeValue' _ c p o (L1 a) = encodeValue p o c     *> gEncodeValue p (o + sizeWord8) a
  gEncodeValue' _ c p o (R1 b) = encodeValue p o (c+1) *> gEncodeValue p (o + sizeWord8) b
  {-# INLINE gEncodeValue' #-}

-- Equivalent to a single value.
instance (Theseus c) => GTheseus (K1 i c) where
  gSizeOfValue = sizeOfValue . unK1
  {-# INLINE gSizeOfValue #-}

  gDecodeValue' _ _ lc b ptr o = first K1 <$> decodeValue lc b ptr o
  {-# INLINE gDecodeValue' #-}

  gEncodeValue' _ _ ptr o = encodeValue ptr o . unK1
  {-# INLINE gEncodeValue' #-}

-- Meta-information
instance (GTheseus f) => GTheseus (M1 i t f) where
  gSizeOfValue = gSizeOfValue . unM1
  {-# INLINE gSizeOfValue #-}

  -- Need to pass through constructor depth for sum-types

  gNumConstructors = gNumConstructors . unMeta
  {-# INLINE gNumConstructors #-}

  gDecodeValue' pr c lc b ptr o = first M1 <$> gDecodeValue' (unMeta pr) c lc b ptr o
  {-# INLINE gDecodeValue' #-}

  gEncodeValue' pr c ptr o = gEncodeValue' (unMeta pr) c ptr o . unM1
  {-# INLINE gEncodeValue' #-}

unMeta :: Proxy ((M1 i t f) a) -> Proxy (f a)
unMeta _ = Proxy

-- Constructors without arguments
instance GTheseus U1 where
  gSizeOfValue = const 0
  {-# INLINE gSizeOfValue #-}

  gDecodeValue' _ _ _ _ _ o = return (U1, o)
  {-# INLINE gDecodeValue' #-}

  gEncodeValue' _ _ _ _ _ = return ()
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
