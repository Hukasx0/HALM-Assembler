module Values where

import Text.Parsec
import Numeric (readHex, readOct, showIntAtBase)
import Data.Word
import Data.Char

type Label = String
type MacroTable = [(String,Value)]
type MLMacroTable = [(String,[Operation])]
type LabelTable = [(String,Int)]
type ShadowTable = [(String,String,[Operation])]

data Value = Register8 String | Register16 String | Int String | Hex String | Oct String | Bin String | Ch Char | Str String 
             | Math String Value Value | UseM String | Pointer String | Deref Label | Ret Operation | Retr Operation
             | Rev Value
                deriving(Eq,Show)

data Operation = Mov Value Value | Interrupt Value | Inc Value | Dec Value | Cmp Value Value | Jmp Label Int 
                | Je Label Int | Jne Label Int | Jg Label Int | Jge Label Int | Jl Label Int | Jle Label Int
                | AdBy [Value] | DoSh String | Comment String | Disp Value | DispA Value 
                | ShowV String Value | DefM String Value | DefMlM String [Operation] | UseMLM String
                | FillB Value Value | Add Value Value | Sub Value Value | Neg Value | Xor Value Value
                | DefLabel String | Incl String | If Value [Operation] | Push Value | Pop Value
                | Shadow String | DefAs String String [Operation] | UseAs String String | SetOrigin Value
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

initLib :: String
initLib = "def as builtIn init = {\nshow str \"Initializing with name $name\"\nif $isWindows {\ndoSh = \"mkdir $filePath\\$name\""
        ++
          "doSh = \"echo include hiLevel.halm > $filePath\\$name\\main.halm\"\ndoSh = \"echo include asm.halm >> $filePath\\$name\\main.halm\"\ndoSh = \"type nul > $filePath\\$name\\asm.halm\""
        ++
          "doSh = \"type nul > $filePath\\$name\\hiLevel.halm\"\nshow str \"$filePath\\$name\\ \"\nshow str \"      main.halm\"\nshow str \"      asm.halm\"\nshow str \"      hiLevel.halm\"\n}"
        ++    
          "if $isUnix {\ndoSh = \"mkdir $filePath/$name\"\ndoSh = \"echo 'include hiLevel.halm' > $filePath/$name/main.halm\"\ndoSh = \"echo 'include asm.halm' >> $filePath/$name/main.halm\""
        ++
          "doSh = \"touch $filePath/$name/asm.halm\"\ndoSh = \"touch $filePath/$name/hiLevel.halm\"show str \"$filePath/$name/\"show str \"      main.halm\"show str \"      asm.halm\""
        ++
          "show str \"      hiLevel.halm\"\n}\n}\n"

libList :: String
libList = "shadow def builtIn\n"++initLib
