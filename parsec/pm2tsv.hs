{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Monad(when)
import System.Environment(getArgs)
import System.Exit
import Text.Parsec
import Text.Parsec.String
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (emptyDef)

data Attribute = Attribute { getName :: String
                           , getValue :: String
                           } deriving (Show)

data Element = Element String [Attribute] [Element]
             | Body String
             deriving (Show)

lexer           = P.makeTokenParser emptyDef

whiteSpace      = P.whiteSpace lexer
identifier      = P.identifier lexer
stringLiteral   = P.stringLiteral lexer

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
    parseTest pReport content