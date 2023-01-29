module Values where

import Text.Parsec
import Numeric (readHex, readOct, showIntAtBase)
import Data.Word
import Data.Char

type Label = String
type MacroTable = [(String,Value)]
type MLMacroTable = [(String,[Operation])]
type MLMPacroTable = [(String,([String],[Operation]))]
type ParamTable = [([String],[Operation])]
type LabelTable = [(String,Int)]
type ShadowTable = [(String,String,[Operation])]

data Value = Register8 String | Register16 String | Int String | Hex String | Oct String | Bin String | Ch Char | Str String 
             | Math String Value Value | UseM String | Pointer String | Deref Label | Ret Operation | Retr Operation
             | Rev Value | Sort Value | SortMany [Value] | RevMany [Value] | Parameter String | FileCon String | Count Value
             | Filter Value [Value]
                deriving(Eq,Show)

data Operation = Mov Value Value | Interrupt Value | Inc Value | Dec Value | Cmp Value Value | Jmp Label Int 
                | Je Label Int | Jne Label Int | Jg Label Int | Jge Label Int | Jl Label Int | Jle Label Int
                | AdBy [Value] | DoSh String | Comment String | Disp Value | DispA Value 
                | ShowV String [Value] | DefM String Value | DefMlM String [Operation] | UseMLM String
                | FillB Value Value | Add Value Value | Sub Value Value | Neg Value | Xor Value Value
                | DefLabel String | Incl String | If Value [Operation] | Push Value | Pop Value
                | Shadow String | DefAs String String [Operation] | UseAs String String | SetOrigin Value
                | DefMlMP String [String] [Operation] | UseMLMP String [Value] | Foreach String [Value] [Operation]
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

word8ToBoolean :: [Word8] -> Bool
word8ToBoolean [] = error $ "Not a boolean"
word8ToBoolean w8s|w8s!!0==1=True
                  |w8s!!0==0=False
                  |otherwise = error $ "Not a boolean"

booleanToWord8List :: Bool -> [Word8]
booleanToWord8List True = [1]
booleanToWord8List False = [0]
booleanToWord8List _ = []

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

readSelfLib :: String
readSelfLib = "def readSelf(type) = {\nif(== 'type \"hex\"){\nshow hexArr reverse (readF = \"$filePath/$fileName.bin\")\n}\nif(== 'type \"oct\"){\nshow octArr reverse (readF = \"$filePath/$fileName.bin\")"
            ++
               "\n}\nif(== 'type \"bin\"){\nshow binArr reverse (readF = \"$filePath/$fileName.bin\")\n}\nif(== 'type \"int\"){\nshow intArr reverse (readF = \"$filePath/$fileName.bin\")\n}\nif(== 'type \"str\"){"
            ++
               "show str (readF = \"$filePath/$fileName.bin\")\n}\nif(== 'type \"char\"){\nshow chars (readF = \"$filePath/$fileName.bin\")\n}\nif (== 'type \"size\"){\nshow int count (readF = \"$filePath/$fileName.bin\")"
            ++
               "\n}\n}\n"

asciiDefinitions :: String
asciiDefinitions = "def asciiLow = 33\ndef asciiHigh = 126\ndef asciiSpace = 10\ndef letterLow = 65\ndef letterHigh = 122\n"

libList :: String
libList = "shadow def builtIn\n"++initLib++readSelfLib++asciiDefinitions
