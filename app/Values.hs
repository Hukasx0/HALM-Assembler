module Values where

import Text.Parsec
import Numeric (readHex, readOct, showIntAtBase)
import Data.Word
import Data.Char

type Label = String
type MacroTable = [(String,Value)]
type MLMacroTable = [(String,[Operation])]
type LabelTable = [(String,Int)]

data Value = Register String | Int String | Hex String | Oct String | Bin String | Ch Char | Str String 
             | Math String Value Value | UseM String | Pointer String
                deriving(Eq,Show)

data Operation = Mov Value Value | Interrupt Value | Inc Value | Dec Value | Cmp Value Value | Jmp Label Int 
                | Je Label Int | Jne Label Int | Jg Label Int | Jge Label Int | Jl Label Int | Jle Label Int
                | AdBy [Value] | DoSh String | Comment String | Disp Value | DispA Value 
                | ShowV String Value | DefM String Value | DefMlM String [Operation] | UseMLM String
                | FillB Value Value | Add Value Value | Sub Value Value | Neg Value | Xor Value Value
                | DefLabel String | Incl String
                deriving(Eq,Show)

letterDigitParser :: Parsec String () Char
letterDigitParser = oneOf ['0'..'9'] <|> oneOf ['a'..'z'] <|> oneOf ['A'..'Z']

getHexFromStr :: String -> Int
getHexFromStr str = fst $ head $ readHex str

getOctFromStr :: String -> Int
getOctFromStr str = fst $ head $ readOct str

getBinFromStr :: String -> Int
getBinFromStr str = sum [2 ^ p | (c, p) <- zip (reverse str) [0..], c == '1']

word8ListToInt :: [Word8] -> Int
word8ListToInt w8s = sum $ zipWith (*) (map fromIntegral w8s) (iterate (*256) 1)

word8ListToString :: [Word8] -> String
word8ListToString w8s = map (chr . fromIntegral) w8s

intToHex :: Int -> String
intToHex x = showIntAtBase 16 intToDigit x ""

intToOct :: Int -> String
intToOct x = showIntAtBase 8 intToDigit x ""

intToBin :: Int -> String
intToBin x = showIntAtBase 2 intToDigit x ""

intToWord8List :: Int -> [Word8]
intToWord8List x
  | x < 0     = error "Cannot convert negative number to Word8 list"
  | x <= 255  = [fromIntegral x]
  | otherwise = fromIntegral (x `mod` 256) : intToWord8List (x `div` 256)

helloLib :: String
helloLib = (  "def hello = {mov ah,0x0e mov al,'H' int 0x10 mov ah,0x0e mov al,'e' int 0x10 mov ah,0x0e mov al,'l' int 0x10 mov ah,0x0e"
                ++
              "mov al,'l'int 0x10 mov ah,0x0e mov al,'o' int 0x10 jmp $ addBytes = (times (- 508 (* 6 5) ) 0x0) addBytes = 0x55,0xaa doSh = \"qemu-system-x86_64 $filePath.bin\"}\n" )

libList :: String
libList = ""--helloLib
