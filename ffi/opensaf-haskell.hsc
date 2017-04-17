{-# LANGUAGE CPP, ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Haskell Opensaf binding prototype
--
-- Dependencies: opensaf-5.2.0
--
-- Build:
--   hsc2hs opensaf-haskell.hsc
--   ghc -o opensaf-haskell opensaf-haskell.hs -lSaImmOm
--
-- Supported functions:
--   - saImmOmInitialize()
--   - saImmOmAccessorInitialize()
--   - saImmOmAccessorGet() (partially)
--     Supported value types:
--       SA_IMM_ATTR_SAINT32T
--       SA_IMM_ATTR_SANAMET
--       SA_IMM_ATTR_SASTRINGT


module Main where

import Control.Monad(when)
import Data.Char
import Foreign
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Array (copyArray)
import Foreign.Ptr
import System.Environment(getArgs)
import System.Exit
import System.IO.Unsafe (unsafePerformIO)

#include <saAis.h>
#include <saImm.h>

-- saAis.h
-- Types used by the NTF/IMMS service
--
newtype SaAisErrorT = SaAisErrorT { getSaAisErrorT :: CInt }
  deriving (Show, Eq)
#{enum SaAisErrorT, SaAisErrorT
  , saAisOk             = SA_AIS_OK
  , saAisErrLibrary     = SA_AIS_ERR_LIBRARY
  , saAisErrVersion     = SA_AIS_ERR_VERSION
  , saAisErrInit        = SA_AIS_ERR_INIT
  }

data SaNameT = SaNameT { saNameLength :: CUShort
                       , saNameValue :: [CUChar] }
  deriving (Show)

data SaVersionT = SaVersionT { releaseCode :: CChar
                             , majorVersion :: CUChar
                             , minorVersion :: CUChar }
  deriving (Show, Read, Eq)


-- saImm.h
-- 4.2.2 Various IMM Service Names
--
newtype SaImmAttrNameT = SaImmAttrNameT  { getSaImmAttrNameT :: CString }
  deriving (Show, Storable)

-- 4.2.3 SaImmValueTypeT
--
newtype SaImmValueTypeT = SaImmValueTypeT { getSaImmValueTypeT :: CInt }
  deriving (Show, Eq, Storable)
#{enum SaImmValueTypeT, SaImmValueTypeT
  , saImmAttrSaInt32    = SA_IMM_ATTR_SAINT32T
  , saImmAtrrSaUint32   = SA_IMM_ATTR_SAUINT32T
  , saImmAttrSaInt64    = SA_IMM_ATTR_SAINT64T
  , saImmAttrSaUint64   = SA_IMM_ATTR_SAUINT64T
  , saImmAttrSaTime     = SA_IMM_ATTR_SATIMET
  , saImmAttrSaName     = SA_IMM_ATTR_SANAMET
  , saImmAttrSaFloat    = SA_IMM_ATTR_SAFLOATT
  , saImmAttrSaDouble   = SA_IMM_ATTR_SADOUBLET
  , saImmAttrSaString   = SA_IMM_ATTR_SASTRINGT
  , saImmAtrrSaAny      = SA_IMM_ATTR_SAANYT
  }

-- 4.2.6 SaImmAttrValueT
--
data SaImmAttrValueT = SaImmAttrSaInt32 { getSaInt32 :: CInt }
                     | SaImmAttrSaUint32 { getSaUint32 :: CUInt }
                     | SaImmAttrSaInt64  { getSaInt64 ::CLong }
                     | SaImmAttrSaUint64  { getSaUint64 ::CULong}
                     | SaImmAttrSaTime { getSaTime :: CLong}
                     | SaImmAttrSaName { getSaName :: SaNameT}
                     | SaImmAttrSaFloat { getSaFloat :: CFloat}
                     | SaImmAttrSaDouble { getSaDouble :: CDouble}
                     | SaImmAttrSaString { getSaString :: CString}
                     | SaImmAttrSaAny { getSaAny :: SaNameT} -- TODO: implement
  deriving (Show)

-- 4.2.8 SaImmAttrValuesT_2
--
data SaImmAttrValuesT_2 = SaImmAttrValuesT_2
    { saImmAttrValuesAttrName :: SaImmAttrNameT
    , saImmAttrValuesAttrValueType :: SaImmValueTypeT
    , saImmAttrValuesAttrValuesNumber :: CUInt
    , saImmAttrValuesAttrValues :: [SaImmAttrValueT] }
  deriving (Show)


-- saImmOm.h
-- 4.2.1 Handles Used by the IMM Service
--
newtype SaImmHandleT = SaImmHandleT { getSaImmHandleT :: CULLong }
  deriving (Show, Storable)

newtype SaImmAccessorHandleT = SaImmAccessorHandleT { getSaImmAccessorHandleT :: CULLong }
  deriving (Show, Storable)

-- 4.3.1 saImmOmInitialize()
--
foreign import ccall "saImmOm.h saImmOmInitialize"
  c_saImmOmInitialize :: Ptr SaImmHandleT
                      -> Ptr ()
                      -> Ptr SaVersionT
                      -> IO SaAisErrorT

saImmOmInitialize :: IO (Either SaAisErrorT SaImmHandleT)
saImmOmInitialize = do
  let version = SaVersionT (CChar . fromIntegral $ ord 'A') 2 15
  alloca $ \handlePtr -> do
  alloca $ \versionPtr -> do
    poke versionPtr version
    res <- c_saImmOmInitialize handlePtr nullPtr versionPtr
    let val = getSaAisErrorT res
    if res == saAisOk
      then do
        handle <- peek handlePtr
        return $ Right handle
      else do
        return $ Left res

-- 4.6.1 saImmOmAccessorInitialize()
--
foreign import ccall "saImmOm.h saImmOmAccessorInitialize"
  c_saImmOmAccessorInitialize :: SaImmHandleT
                              -> Ptr SaImmAccessorHandleT
                              -> IO SaAisErrorT

saImmOmAccessorInitialize :: SaImmHandleT -> IO (Either SaAisErrorT SaImmAccessorHandleT)
saImmOmAccessorInitialize handle = do
  alloca $ \accHandlePtr -> do
    res <- c_saImmOmAccessorInitialize handle accHandlePtr
    let val = getSaAisErrorT res
    if res == saAisOk
      then do
        handle <- peek accHandlePtr
        return $ Right handle
      else do
        return $ Left res

-- 4.6.2 saImmOmAccessorGet()
--
foreign import ccall "saImmOm.h saImmOmAccessorGet_2"
  c_saImmOmAccessorGet_2 :: SaImmAccessorHandleT
                         -> Ptr SaNameT
                         -> Ptr SaImmAttrNameT
                         -> Ptr (Ptr (Ptr SaImmAttrValuesT_2))
                         -> IO SaAisErrorT

saImmOmAccessorGet :: SaImmAccessorHandleT
                   -> String
                   -> IO (Either SaAisErrorT  [SaImmAttrValuesT_2])
saImmOmAccessorGet handle object = do
  alloca $ \saname -> do
  alloca $ \attributes -> do
    let newobj = SaNameT (fromIntegral (length object)) (fmap castCharToCUChar object)
    poke saname newobj
    st <- peek saname
    res <- c_saImmOmAccessorGet_2 handle saname nullPtr attributes
    let val = getSaAisErrorT res
    if res == saAisOk
      then do
        attrs <- peek attributes
        attrList <- peekArray0 nullPtr attrs
        let result = map (unsafePerformIO . peek) attrList
        return $ Right result
      else do
        return $ Left res

--
-- Marshaling
--

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

instance Storable SaVersionT where
  alignment _ = #{alignment SaVersionT}
  sizeOf _ = #{size SaVersionT}
  peek ptr = do
    releaseCode <- #{peek SaVersionT, releaseCode} ptr
    majorVersion <- #{peek SaVersionT, majorVersion} ptr
    minorVersion <- #{peek SaVersionT, minorVersion} ptr
    return (SaVersionT releaseCode majorVersion minorVersion)
  poke ptr (SaVersionT releaseCode majorVersion minorVersion) = do
    #{poke SaVersionT, releaseCode} ptr releaseCode
    #{poke SaVersionT, majorVersion} ptr majorVersion
    #{poke SaVersionT, minorVersion} ptr minorVersion


peekArraySaInt32 :: Int -> Ptr CInt -> IO [CInt]
peekArraySaInt32 size ptr | size <= 0 = return []
                     | otherwise = f (size-1) []
  where
    f 0 acc = do e <- peekElemOff ptr 0; return (e:acc)
    f n acc = do e <- peekElemOff ptr n; f (n-1) (e:acc)

peekArraySaNameT :: Int -> Ptr SaNameT -> IO [SaNameT]
peekArraySaNameT size ptr | size <= 0 = return []
                     | otherwise = f (size-1) []
  where
    f 0 acc = do e <- peekElemOff ptr 0; return (e:acc)
    f n acc = do e <- peekElemOff ptr n; f (n-1) (e:acc)

peekArraySaString :: Int -> Ptr CString -> IO [CString]
peekArraySaString size ptr | size <= 0 = return []
                     | otherwise = f (size-1) []
  where
    f 0 acc = do e <- peekElemOff ptr 0; return (e:acc)
    f n acc = do e <- peekElemOff ptr n; f (n-1) (e:acc)

instance Storable SaImmAttrValuesT_2 where
  alignment _ = #{alignment SaImmAttrValuesT_2}
  sizeOf _ = #{size SaImmAttrValuesT_2}
  peek ptr = do
    saImmAttrValuesAttrName         <- #{peek SaImmAttrValuesT_2, attrName} ptr
    saImmAttrValuesAttrValueType    <- #{peek SaImmAttrValuesT_2, attrValueType} ptr
    saImmAttrValuesAttrValuesNumber <- #{peek SaImmAttrValuesT_2, attrValuesNumber} ptr

    case saImmAttrValuesAttrValueType of
      attrType | attrType == saImmAttrSaInt32 -> do
        saImmAttrValuesAttrValues <- (map SaImmAttrSaInt32) <$> (peekArraySaInt32 (fromIntegral saImmAttrValuesAttrValuesNumber) (plusPtr ptr 16) )
        return (SaImmAttrValuesT_2 saImmAttrValuesAttrName
                                   saImmAttrValuesAttrValueType
                                   saImmAttrValuesAttrValuesNumber
                                   saImmAttrValuesAttrValues)
               | attrType == saImmAttrSaName -> do
        saImmAttrValuesAttrValues <- (map SaImmAttrSaName) <$> (peekArraySaNameT (fromIntegral saImmAttrValuesAttrValuesNumber) (plusPtr ptr 16) )
        return (SaImmAttrValuesT_2 saImmAttrValuesAttrName
                                   saImmAttrValuesAttrValueType
                                   saImmAttrValuesAttrValuesNumber
                                   saImmAttrValuesAttrValues)
               | attrType == saImmAttrSaString -> do
        saImmAttrValuesAttrValues <- (map SaImmAttrSaString) <$> (peekArraySaString (fromIntegral saImmAttrValuesAttrValuesNumber) (plusPtr ptr 16) )
        return (SaImmAttrValuesT_2 saImmAttrValuesAttrName
                                   saImmAttrValuesAttrValueType
                                   saImmAttrValuesAttrValuesNumber
                                   saImmAttrValuesAttrValues)
  poke ptr (SaImmAttrValuesT_2 saImmAttrValuesAttrName
                               saImmAttrValuesAttrValueType
                               saImmAttrValuesAttrValuesNumber
                               saImmAttrValuesAttrValues) = do
    #{poke SaImmAttrValuesT_2, attrName} ptr saImmAttrValuesAttrName
    #{poke SaImmAttrValuesT_2, attrValueType} ptr saImmAttrValuesAttrValueType
    #{poke SaImmAttrValuesT_2, attrValuesNumber} ptr saImmAttrValuesAttrValuesNumber

    case saImmAttrValuesAttrValues of
      [SaImmAttrSaInt32 val] -> do
        pokeArray (plusPtr ptr 16) $  map getSaInt32 saImmAttrValuesAttrValues
      [SaImmAttrSaName val] -> do
        pokeArray (plusPtr ptr 16) $  map getSaName saImmAttrValuesAttrValues
      [SaImmAttrSaString val] -> do
        pokeArray (plusPtr ptr 16) $  map getSaString saImmAttrValuesAttrValues

--
-- Helper functions
--

initializeImm :: IO (Either String (SaImmHandleT, SaImmAccessorHandleT))
initializeImm = do
  res <- saImmOmInitialize
  case res of
    Right handle -> do
      res <- saImmOmAccessorInitialize handle
      case res of
        Right accHandle -> do
          return $ Right (handle, accHandle)
        Left err -> do
          return $ Left "IMM error: failed to initialize accessor handle"
    Left err -> do
      return $ Left "IMM error: failed to initialize handle"

main = do
  args <- getArgs
  when (length args /= 1) $ do
      putStrLn "Syntax: opensaf-haskell <mo>"
      exitFailure
  res <- initializeImm
  case res of
    Right (_, accessorHandle)  -> do
      print "IMM OK"
      retval <- saImmOmAccessorGet accessorHandle (head args)
      case retval of
        Right attribs -> do
          print attribs
        Left err -> print err
    Left x -> print x
