module Main where

--import System.Environment
--import Data.List
import Text.Parsec
import Text.Parsec.String


type Register = String
type Label = String
type RegOrVal = String

data Operation = Mov Register RegOrVal | Interrupt String | Inc Register | Dec Register | Cmp Register RegOrVal | Jmp Label
                deriving(Eq,Show)

letterDigitParser :: Parsec String () Char
letterDigitParser = oneOf ['0'..'9'] <|> oneOf ['a'..'z'] <|> oneOf ['A'..'Z']

movParser :: Parser Operation
movParser = Mov <$>  (string "mov" >> many1 space >> many1 letter) <*> (char ',' >> spaces >> many1 letterDigitParser)

interruptParser :: Parser Operation
interruptParser = Interrupt <$> (string "int" >> many1 space >> many1 letterDigitParser)

incParser :: Parser Operation
incParser = Inc <$> (string "inc" >> many1 space >> many1 letterDigitParser)

decParser :: Parser Operation
decParser = Dec <$> (string "dec" >> many1 space >> many1 letterDigitParser)

jmpParser :: Parser Operation
jmpParser = Jmp <$> (string "jmp" >> many1 space >> many1 letterDigitParser)

cmpParser :: Parser Operation
cmpParser = Cmp <$>  (string "cmp" >> many1 space >> many1 letter) <*> (char ',' >> spaces >> many1 letterDigitParser)

finalParser :: Parser Operation
finalParser = try movParser <|> try interruptParser <|> try incParser <|> try decParser <|> try cmpParser <|> try jmpParser

insToBin :: Operation -> String
insToBin (Mov a b) = "copying value from "++b++" to "++a
insToBin (Interrupt code) = "interrupting with code "++code
insToBin (Inc reg) = "incrementing "++reg++" value"
insToBin (Dec reg) = "decrementing "++reg++" value"
insToBin (Cmp a b) = "comparing "++a++" with "++b
insToBin (Jmp l) = "unconditional jump to "++l

codeToIns :: [Operation] -> String
codeToIns code = unlines $ map insToBin code

virtualFile :: String
virtualFile = "mov al,123\nmov bl,0xA\nmov dl,al\ninc al\ndec dl\ncmp al,123\nint 0x10\njmp test\n"

main :: IO ()
main = do putStrLn $ ("input:\n"++virtualFile) 
          let parsed = parse (spaces >> many (finalParser <* many1 space) <* eof) "virtual" virtualFile
          putStrLn $ "output:"
          case parsed of
            Left err -> print err
            Right corr -> putStrLn (codeToIns $ corr)
 
