module Values where

import Text.Parsec
import Numeric (readHex)

type Label = String

data Value = Register String | Int String | Hex String | Ch Char
                deriving(Eq,Show)

data Operation = Mov Value Value | Interrupt Value | Inc Value | Dec Value | Cmp Value Value | Jmp Label | AdBy [Value]
                deriving(Eq,Show)

letterDigitParser :: Parsec String () Char
letterDigitParser = oneOf ['0'..'9'] <|> oneOf ['a'..'z'] <|> oneOf ['A'..'Z']

getHexFromStr :: String -> Int
getHexFromStr str = fst $ head $ readHex str

virtualFile :: String
virtualFile = "addBytes = 0xff,123,0xA \nmov al,123\nmov bl,0xA\nmov dl,al\ninc al\ndec dl\ncmp al,123\nint 0x10\njmp test\n"
