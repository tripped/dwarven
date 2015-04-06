{-# LANGUAGE OverloadedStrings #-}
import System.Environment
import Data.Char
import Data.Word
import qualified Data.ByteString as B

-- The Dwarf Fortress character set, which is really just codepage 437.
dfchars :: String
dfchars = 
    " ☺☻♥♦♣♠•◘○◙♂♀♪♫☼" ++
    "►◄↕‼¶§▬↨↑↓→←∟↔▲▼" ++
    " !\"#$%&'()*+,-./"++
    "0123456789:;<=>?" ++
    "@ABCDEFGHIJKLMNO" ++
    "PQRSTUVWXYZ[\\]^_"++
    "`abcdefghijklmno" ++
    "pqrstuvwxyz{|}~⌂" ++
    "ÇüéâäàåçêëèïîìÄÅ" ++
    "ÉæÆôöòûùÿÖÜ¢£¥₧ƒ" ++
    "áíóúñÑªº¿⌐¬½¼¡«»" ++
    "░▒▓│┤╡╢╖╕╣║╗╝╜╛┐" ++
    "└┴┬├─┼╞╟╚╔╩╦╠═╬╧" ++
    "╨╤╥╙╘╒╓╫╪┘┌█▄▌▐▀" ++
    "αßΓπΣσµτΦΘΩδ∞φε∩" ++
    "≡±≥≤⌠⌡÷≈°∙·√ⁿ²■ "

dfToUnicode :: Word8 -> Char
dfToUnicode c = dfchars !! (fromIntegral c)

-- A specialized character conversion for partially-encoded raw files.
-- Rather than parse the raws to figure out which parts are CP437, we
-- make two complementary and most likely valid assumptions:
--    1. The CP437 parts do not contain codes 9, 10 or 13.
--    2. The rest of the file is printable ASCII and codes 9, 10 or 13.
-- Using this function to decode a DF language raw should safely produce
-- a correct raw in which the CP437 characters have been unicodified.
rawToUnicode :: Word8 -> Char
rawToUnicode c
    | c `elem` [9, 10, 13] = chr $ fromIntegral c
    | otherwise            = dfToUnicode c

main :: IO ()
main = do
    args <- getArgs
    contents <- B.readFile $ head args
    putStr $ map rawToUnicode $ B.unpack contents
