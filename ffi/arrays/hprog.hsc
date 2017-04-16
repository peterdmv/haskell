{-# LANGUAGE CPP, ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Foreign
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr

#include "array.h"

data SaNameT = SaNameT { saNameLength :: CUShort
                       , saNameValue :: [CUChar] }
  deriving (Show)

instance Storable SaNameT where
  alignment _ = #{alignment SaNameT}
  sizeOf _ = #{size SaNameT}
  peek ptr = do
    saNameLength <- #{peek SaNameT, length} ptr
    saNameValue <- peekArray (fromIntegral saNameLength) (plusPtr ptr (sizeOf saNameLength))
    return (SaNameT saNameLength saNameValue)
  poke ptr (SaNameT saNameLength saNameValue) = do
    #{poke SaNameT, length} ptr saNameLength
    pokeArray (plusPtr ptr (sizeOf saNameLength)) saNameValue

foreign import ccall "array.h foo"
  c_foo :: Ptr SaNameT -> IO CInt

main = do
  alloca $ \saname -> do
    let input = "hello"
    let newobj = SaNameT (fromIntegral (length input)) (fmap castCharToCUChar input)
    poke saname newobj
    st <- peek saname
    print st
    c_foo saname
    return 0
