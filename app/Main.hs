module Main where

import System.Environment
import Data.List
import Text.Parsec
import Text.Parsec.String


type Register = String
data Operation = Mov Register Register | Interrupt String
                deriving(Eq,Show)

letterDigitParser :: Parsec String () Char
letterDigitParser = oneOf ['0'..'9'] <|> oneOf ['a'..'z'] <|> oneOf ['A'..'Z']

movParser :: Parser Operation
movParser = Mov <$>  (string "mov" >> many1 space >> many1 letter) <*> (char ',' >> many1 space >> many1 letterDigitParser)

interruptParser :: Parser Operation
interruptParser = Interrupt <$> (string "int" >> many1 space >> many1 letterDigitParser)

finalParser :: Parser Operation
finalParser = try movParser <|> interruptParser

virtualFile :: String
virtualFile = "mov al, 123\n mov bl, 0xA\n mov dl, al\n int 0x10 "

main :: IO ()
main = print $ parse (spaces >> many (finalParser <* many1 space) <* eof) "virtual" virtualFile