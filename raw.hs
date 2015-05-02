{-# LANGUAGE OverloadedStrings #-}
import Prelude hiding (readFile)
import Text.ParserCombinators.Parsec hiding (GenParser)
import System.Environment
import Text.Parsec.Text
import Data.Text hiding (head)
import Data.Text.IO hiding (putStrLn)
import Control.Applicative ((<$>), (*>), (<*))

bracketThing :: GenParser st Text
bracketThing = do
    char '['
    r <- many $ noneOf "]"
    char ']'
    return $ pack r

bracketThing' :: GenParser st Text
bracketThing' = char '[' >>= (\_ -> ((many $ noneOf "]") >>= (\r -> ((char ']') >>= (\_ -> return $ pack r)))))

bracketThingAp :: GenParser st Text
bracketThingAp = pack <$> (char '[' *> (many $ noneOf "]") <* char ']')


ignore = skipMany $ noneOf "["

rawFile = do
    r <- many $ (ignore >> bracketThing) <|> ignore
    return r

--parseRAW :: Text -> Either ParseError [[Text]]
--parseRAW input = parse rawFile "(whocares)" input

main :: IO ()
main = do
    parseTest rawFile "[foobar]"
    parseTest rawFile " barf  [foobar]"
    parseTest rawFile "  crap  [foobar] fdsggr  [blarg]   "

--    args <- getArgs
--    contents <- readFile $ head args
--    putStrLn $ show $ parseCSV contents
