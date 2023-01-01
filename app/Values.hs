module Values where

import Text.Parsec
import Numeric (readHex, readOct)
import Data.Word

type Label = String
type MacroTable = [(String,Value)]
type MLMacroTable = [(String,[Operation])]

data Value = Register String | Int String | Hex String | Oct String | Bin String | Ch Char | Str String 
             | Math String Value Value | UseM String
                deriving(Eq,Show)

data Operation = Mov Value Value | Interrupt Value | Inc Value | Dec Value | Cmp Value Value | Jmp Label 
                | Je Label | Jne Label | Jg Label | Jge Label | Jl Label | Jle Label
                | AdBy [Value] | DoSh String | Comment String | Disp Value | DispA Value 
                | ShowV Value | DefM String Value | DefMlM String [Operation] | UseMLM String
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

intToWord8List :: Int -> [Word8]
intToWord8List x
  | x < 0     = error "Cannot convert negative number to Word8 list"
  | x <= 255  = [fromIntegral x]
  | otherwise = fromIntegral (x `mod` 256) : intToWord8List (x `div` 256)

virtualFile :: String
virtualFile = "addBytes = 0xff,123,0xA \nmov al,123\nmov bl,0xA\nmov dl,al\ninc al\ndec dl\ncmp al,123\nint 0x10\njmp test\n"

helloLib :: String
helloLib = (  "def hello = {mov ah,0x0e mov al,'H' int 0x10 mov ah,0x0e mov al,'e' int 0x10 mov ah,0x0e mov al,'l' int 0x10 mov ah,0x0e"
                ++
              "mov al,'l'int 0x10 mov ah,0x0e mov al,'o' int 0x10 jmp $ addBytes = (times (- 508 (* 6 5) ) 0x0) addBytes = 0x55,0xaa doSh = \"qemu-system-x86_64 $filePath.bin\"}\n" )

libList :: String
libList = helloLib
