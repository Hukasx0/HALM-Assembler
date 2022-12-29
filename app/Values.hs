module Values where

import Text.Parsec
import Numeric (readHex)
import Data.Word

type Label = String
type MacroTable = [(String,Value)]

data Value = Register String | Int String | Hex String | Ch Char | Str String | Math String Value Value | UseM String
                deriving(Eq,Show)

data Operation = Mov Value Value | Interrupt Value | Inc Value | Dec Value | Cmp Value Value | Jmp Label 
                | AdBy [Value] | DoSh String | Comment String | Disp Value | DispA Value | DefM String Value
                deriving(Eq,Show)

letterDigitParser :: Parsec String () Char
letterDigitParser = oneOf ['0'..'9'] <|> oneOf ['a'..'z'] <|> oneOf ['A'..'Z']

getHexFromStr :: String -> Int
getHexFromStr str = fst $ head $ readHex str

word8ListToInt :: [Word8] -> Int
word8ListToInt w8s = sum $ zipWith (*) (map fromIntegral w8s) (iterate (*256) 1)

intToWord8List :: Int -> [Word8]
intToWord8List x
  | x < 0     = error "Cannot convert negative number to Word8 list"
  | x <= 255  = [fromIntegral x]
  | otherwise = fromIntegral (x `mod` 256) : intToWord8List (x `div` 256)

virtualFile :: String
virtualFile = "addBytes = 0xff,123,0xA \nmov al,123\nmov bl,0xA\nmov dl,al\ninc al\ndec dl\ncmp al,123\nint 0x10\njmp test\n"
