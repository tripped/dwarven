{-# LANGUAGE OverloadedStrings #-}
import Prelude hiding (readFile)
import System.Environment
import Text.Parsec.Text (Parser)
import Text.Parsec
import Data.Text hiding (head)
import Data.Text.IO hiding (putStrLn)
import Control.Applicative ((<$>), (*>), (<*))

bracketThing :: Parser Text
bracketThing = do
    char '['
    r <- many $ noneOf "]"
    char ']'
    return $ pack r

ignore = skipMany $ noneOf "["

rawFile = many $ do
    ignore
    r <- bracketThing
    ignore
    return r


parseRAW :: Text -> Either ParseError [Text]
parseRAW input = parse rawFile "(whocares)" input

main :: IO ()
main = do
--    parseTest rawFile "[foobar]"
--    parseTest rawFile " barf  [foobar]"
--    parseTest rawFile "  crap  [foobar] fdsggr  [blarg]   "
    args <- getArgs
    contents <- readFile $ head args
    putStrLn $ show $ parseRAW contents
