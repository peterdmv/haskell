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
--       SA_IMM_ATTR_SAUINT32T
--       SA_IMM_ATTR_SASTRINGT


module Main where

import Control.Monad(when)
import qualified Data.ByteString.Char8 as B
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

-- Native Haskell result types
--
data Value = AVInt32 { getInt32 :: Int32 }
           | AVUint32 { getUint32 :: Word32 }
           | AVInt64  { getInt64 :: Int64 }
           | AVUint64  { getUint64 :: Word64 }
           | AVTime { getTime :: Int64}
           | AVName { getName :: String}
           | AVFloat { getFloat :: Float}
           | AVDouble { getDouble :: Double}
           | AVString { getString :: String}
           | AVAny { getAny :: B.ByteString} -- TODO: implement
  deriving (Show)

data Attribute = Attribute
    { attrName :: String
    , attrValues :: [Value] }
  deriving (Show)

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
type SaImmAttrNameT = CString

-- 4.2.3 SaImmValueTypeT
--
newtype SaImmValueTypeT = SaImmValueTypeT { getSaImmValueTypeT :: CInt }
  deriving (Show, Eq, Storable)
#{enum SaImmValueTypeT, SaImmValueTypeT
  , saImmAttrSaInt32    = SA_IMM_ATTR_SAINT32T
  , saImmAttrSaUint32   = SA_IMM_ATTR_SAUINT32T
  , saImmAttrSaInt64    = SA_IMM_ATTR_SAINT64T
  , saImmAttrSaUint64   = SA_IMM_ATTR_SAUINT64T
  , saImmAttrSaTime     = SA_IMM_ATTR_SATIMET
  , saImmAttrSaName     = SA_IMM_ATTR_SANAMET
  , saImmAttrSaFloat    = SA_IMM_ATTR_SAFLOATT
  , saImmAttrSaDouble   = SA_IMM_ATTR_SADOUBLET
  , saImmAttrSaString   = SA_IMM_ATTR_SASTRINGT
  , saImmAttrSaAny      = SA_IMM_ATTR_SAANYT
  }

-- 4.2.6 SaImmAttrValueT
--
type SaImmAttrValueT = Ptr ()

-- 4.2.8 SaImmAttrValuesT_2
--
data SaImmAttrValuesT_2 = SaImmAttrValuesT_2
    { saImmAttrValuesAttrName :: SaImmAttrNameT
    , saImmAttrValuesAttrValueType :: SaImmValueTypeT
    , saImmAttrValuesAttrValuesNumber :: CUInt
    , saImmAttrValuesAttrValues :: Ptr SaImmAttrValueT }
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
                   -> IO (Either SaAisErrorT  (Ptr (Ptr SaImmAttrValuesT_2)))
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
        return $ Right attrs
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


instance Storable SaImmAttrValuesT_2 where
  alignment _ = #{alignment SaImmAttrValuesT_2}
  sizeOf _ = #{size SaImmAttrValuesT_2}
  peek ptr = do
    saImmAttrValuesAttrName         <- #{peek SaImmAttrValuesT_2, attrName} ptr
    saImmAttrValuesAttrValueType    <- #{peek SaImmAttrValuesT_2, attrValueType} ptr
    saImmAttrValuesAttrValuesNumber <- #{peek SaImmAttrValuesT_2, attrValuesNumber} ptr
    saImmAttrValuesAttrValues       <- #{peek SaImmAttrValuesT_2, attrValues} ptr
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
    #{poke SaImmAttrValuesT_2, attrValues} ptr saImmAttrValuesAttrValues

--
-- Helper functions
--

convertAttrValues :: Ptr (Ptr SaImmAttrValuesT_2) -> [Attribute]
convertAttrValues ptr | ptr == nullPtr = []
convertAttrValues ptr = unsafePerformIO $ do
  attribs <- peekArray0 nullPtr ptr
  let attribs' = map (unsafePerformIO . peek) attribs
  return $ convertAttrValues' attribs'

convertAttrValues' :: [SaImmAttrValuesT_2] -> [Attribute]
convertAttrValues' [] = []
convertAttrValues' (x:xs) =
  let avType = saImmAttrValuesAttrValueType x
      size = saImmAttrValuesAttrValuesNumber x
      values = saImmAttrValuesAttrValues x
  in Attribute { attrName = fetchString . saImmAttrValuesAttrName $ x
             , attrValues = convertValues avType size values } : convertAttrValues' xs


convertValues :: SaImmValueTypeT -> CUInt -> Ptr SaImmAttrValueT -> [Value]
convertValues avType size ptr
   | ptr == nullPtr || size == 0 = []
convertValues avType size ptr = unsafePerformIO $ do
  values <- peekArray (fromIntegral size) ptr
  return $ convertValues' avType values

convertValues' :: SaImmValueTypeT -> [SaImmAttrValueT] -> [Value]
convertValues' avType [] = []
convertValues' avType (x:xs) =
  case avType of
    _ | avType == saImmAttrSaInt32 -> (unsafePerformIO $ do
      value <- peek ((castPtr x) :: Ptr CInt)
      return (AVInt32 $ fromIntegral value)) : convertValues' avType xs
      | avType == saImmAttrSaUint32 -> (unsafePerformIO $ do
      value <- peek ((castPtr x) :: Ptr CUInt)
      return (AVUint32 $ fromIntegral value)) : convertValues' avType xs
      | avType == saImmAttrSaString -> (unsafePerformIO $ do
      ptr <- peek ((castPtr x) :: Ptr CString)
      value <- peekCString ptr
      return (AVString $ value)) : convertValues' avType xs
    _ -> []


fetchString :: CString -> String
fetchString cstr = unsafePerformIO $ peekCString cstr


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


getMO :: SaImmAccessorHandleT -> String -> [Attribute]
getMO handle dn = unsafePerformIO $ do
  retval <- saImmOmAccessorGet handle dn
  case retval of
    Right attribs -> do
      return $ convertAttrValues attribs
    Left err -> do return []


main = do
  args <- getArgs
  when (length args /= 1) $ do
      putStrLn "Syntax: opensaf-haskell <mo>"
      exitFailure
  res <- initializeImm
  case res of
    Right (_, accessorHandle)  -> do
      print "IMM OK"
      print $ getMO accessorHandle (head args)
    Left x -> print x
