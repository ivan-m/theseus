{-# LANGUAGE ScopedTypeVariables #-}

{- |
   Module      : Data.Theseus
   Description : Encode and decode values
   Copyright   : Ivan Lazar Miljenovic, Patryk Zadarnowski
   License     : MIT
   Maintainer  : Ivan.Miljenovic@gmail.com

   Ariadne gave Theseus a <https://en.wiktionary.org/wiki/clew clew>
   to help him navigate through the labyrinth.

   To navigate through your journey through the labyrinth of encoding
   your custom data type:

   1) Create a <https://wiki.haskell.org/GHC.Generics Generics>
      instance (@-XDeriveGeneric@ can help with this)

   2) Derive an instance of 'Theseus' (@-XDeriveAnyClass@ can be used for this)

   3) 'ravel' your data to encode it as a 'ByteString'.

   4) Conquer the Minotaur (optional)

   5) 'unravel' the 'ByteString' to get your data back.

 -}
module Data.Theseus
  ( ravel
  , unravel
  , Theseus
  , TheseusException (..)
  ) where

import Data.Theseus.Values

import           Data.ByteString          (ByteString)
import qualified Data.ByteString.Internal as B

import Control.Exception  (Exception(..), throwIO, try)
import Control.Monad      (void, when)
import Data.Bool          (bool)
import Data.Proxy         (Proxy(..))
import Foreign.ForeignPtr
import Foreign.Ptr
import System.IO.Unsafe   (unsafeDupablePerformIO)

--------------------------------------------------------------------------------

-- | Pull apart a value and convert it to a ByteString.
ravel :: (Theseus a) => a -> ByteString
ravel a = B.unsafeCreate sz $ \p ->
  void (encodeValue p a)
  where
    sz = sizeOfValue a

-- | Separate out the individual bytes from a ByteString and try to
--   construct a value out of it.
unravel :: forall a. (Theseus a) => ByteString -> Either TheseusException a
unravel bs = unsafeDupablePerformIO . try . withForeignPtr fp $ \p -> do
  lc (minSize (Proxy :: Proxy a))
  let p' = plusPtr p off
  fst <$> decodeValue lc bs p' (0,0)
  where
    (fp, off, sz) = B.toForeignPtr bs
    lc len = when (len > sz) (throwIO (TheseusException len sz))

data TheseusException = TheseusException
  { requiredBytes :: Int
  , lengthInBytes :: Int
  } deriving (Eq, Show)

instance Exception TheseusException where
  displayException te
    = unwords [ "Premature end of data with"
              , show (lengthInBytes te)
              , bool "byte" "bytes" (lengthInBytes te > 1)
              , "available, with"
              , show (requiredBytes te)
              , "required."
              ]
