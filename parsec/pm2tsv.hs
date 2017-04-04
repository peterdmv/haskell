{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Monad(forM, forM_, when)
import Data.List (find, intercalate)
import qualified Control.Monad.State as S
import System.Environment(getArgs)
import qualified Data.Map.Strict as Map
import System.Exit
import Text.Parsec
import Text.Parsec.String
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (emptyDef)

data Attribute = Attribute { attrName :: String
                           , attrValue :: String
                           } deriving (Show)

data XML = Element { elemName :: String
                   , elemAttrs :: [Attribute]
                   , elemChildren :: [XML] }
         | Body    { bodyContent :: String }
         deriving (Show)

lexer           = P.makeTokenParser emptyDef

whiteSpace      = P.whiteSpace lexer
identifier      = P.identifier lexer
stringLiteral   = P.stringLiteral lexer

type  MeasTypesMap = Map.Map String String
type ConverterValue = [[String]]
data ConverterState = ConvState { beginTime :: String
                                , endTime :: String
                                , period :: String
                                , measInfoId :: String
                                , jobId :: String
                                , measObjLdn :: String
                                , measTypes :: MeasTypesMap
                                , result :: ConverterValue
                                } deriving (Show)

runConverter :: XML -> S.State ConverterState ConverterValue
runConverter xml = do
  case xml of
    Element _ _ _ -> processElement xml
    Body _        -> processBody xml
  state <- S.get
  return $ reverse (result state)

processElement xml = case elemName xml of
   "measInfo"   -> processMeasInfo xml
   "measType"   -> processMeasType xml
   "measCollec" -> processMeasCollec xml
   "granPeriod" -> processGranPeriod xml
   "job"        -> processJob xml
   "measValue"  -> processMeasValue xml
   "r"          -> processR xml
   _ -> processChildren $ elemChildren xml

processMeasInfo xml = do
  state <- S.get
  S.put state { endTime = ""
              , period = ""
              , measInfoId = ""
              , jobId = ""
              , measTypes = Map.empty }
  case findAttrib (elemAttrs xml) "measInfoId" of
    Nothing -> processChildren $ elemChildren xml
    Just attribute -> do
        S.put state { measInfoId = attrValue attribute }
        processChildren $ elemChildren xml

processMeasType xml = do
  state <- S.get
  case findAttrib (elemAttrs xml) "p" of
    Nothing -> processChildren $ elemChildren xml
    Just attribute -> do
        let measTypesMap = measTypes state
            key = attrValue attribute
            value = bodyContent . head . elemChildren $ xml
            newMap = Map.insert key value $ measTypesMap
        S.put state { measTypes = newMap }
        processChildren $ elemChildren xml

processMeasCollec xml = do
  state <- S.get
  case findAttrib (elemAttrs xml) "beginTime" of
    Nothing -> processChildren $ elemChildren xml
    Just attribute -> do
        S.put state { beginTime = attrValue attribute }
        processChildren $ elemChildren xml

processGranPeriod xml = do
  case findAttrib (elemAttrs xml) "duration" of
    Nothing -> return [[]]
    Just attribute -> do
        state <- S.get
        S.put state { period = attrValue attribute }
        return [[]]
  case findAttrib (elemAttrs xml) "endTime" of
    Nothing -> return [[]]
    Just attribute -> do
        state <- S.get
        S.put state { endTime = attrValue attribute }
        return [[]]
  processChildren $ elemChildren xml

processJob xml = do
  state <- S.get
  case findAttrib (elemAttrs xml) "jobId" of
    Nothing -> processChildren $ elemChildren xml
    Just attribute -> do
        S.put state { jobId = attrValue attribute }
        processChildren $ elemChildren xml

processMeasValue xml = do
  state <- S.get
  S.put state { measObjLdn = "" }
  case findAttrib (elemAttrs xml) "measObjLdn" of
    Nothing -> processChildren $ elemChildren xml
    Just attribute -> do
        S.put state { measObjLdn = attrValue attribute }
        processChildren $ elemChildren xml

processR xml = do
  state <- S.get
  case findAttrib (elemAttrs xml) "p" of
    Nothing -> return [[]]
    Just attribute -> do
      let measTypesMap = measTypes state
          key = attrValue attribute
          value = bodyContent . head . elemChildren $ xml
          measType = Map.lookup key $ measTypesMap
      case measType of
        Just name -> do
          S.put state { result = (createRow state name value) : (result state) }
          return [[]]
        Nothing -> return [[]]

createRow :: ConverterState -> String -> String -> [String]
createRow state name value =
    [measInfoId state, jobId state, period state,
     beginTime state, endTime state, name, value]

processBody xml = return [[]]

processChildren :: [XML] -> S.State ConverterState ConverterValue
processChildren [] = return [[]]
processChildren (x:xs) = do
  forM (x:xs) runConverter
  return [[]]

findAttrib attrList attr = find ((== attr) . attrName) attrList


pXmlDecl :: Parser String
pXmlDecl = try (string "<?") *> many (noneOf "?>") <* string "?>" <* whiteSpace

pTag = do
  name <- try $ do
    char '<'
    identifier
  whiteSpace
  attributes <- many pAttribute
  close <- try (string "/>" <|> string ">")
  if (length close) == 2
  then return (Element name attributes [])
  else do
    elementBody <- many pElementBody
    pEndTag name
    whiteSpace
    return (Element name attributes elementBody)

pNsIdentifier = do
  ns <- try $ do
    identifier <* char ':'
  name <- identifier
  return (concat [ns, ":", name])

pElementBody = pTag <|> pText

pText = Body <$> many1 (noneOf "><")

pEndTag str = string "</" *> string str <* char '>'

pAttribute = do
  name <- pNsIdentifier <|>  identifier
  whiteSpace >> char '=' >> whiteSpace
  value <- stringLiteral
  whiteSpace
  return (Attribute name value)

pReport = many pXmlDecl *> many pTag

main = do
    args <- getArgs
    when (length args /= 1) $ do
        putStrLn "Syntax: pm2tsv filename"
        exitFailure
    content <- readFile (head args)
    let (Right [ast]) = parse pReport "" content
        start = ConvState "" "" "" "" "" "" (Map.empty) []
        rows = S.evalState (runConverter ast) start
    forM_ rows $ putStrLn . (intercalate "\t")