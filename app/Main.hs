module Main where

--import System.Environment
--import Data.List
import Text.Parsec
import Text.Parsec.String


type Label = String

data Value = Register String | Int String | Hex String
                deriving(Eq,Show)

data Operation = Mov Value Value | Interrupt Value | Inc Value | Dec Value | Cmp Value Value | Jmp Label
                deriving(Eq,Show)

letterDigitParser :: Parsec String () Char
letterDigitParser = oneOf ['0'..'9'] <|> oneOf ['a'..'z'] <|> oneOf ['A'..'Z']

-- value parsers

registerParser :: Parser Value
registerParser = Register <$> (string "al" <|> string "bl" <|> string "cl" <|> string "dl")

intParser :: Parser Value
intParser = Int <$> (many1 digit)

hexParser :: Parser Value
hexParser = Hex <$> (string "0x" >> many1 hexDigit)

anyValParser :: Parser Value
anyValParser = try registerParser <|> try hexParser <|> intParser

onlyValParser :: Parser Value
onlyValParser = try hexParser <|> intParser

valToBin :: Value -> String
valToBin(Register a) = "(register "++a++")"
valToBin(Int b) = "(Integer value "++b++")"
valToBin(Hex c) = "(Hexadecimal value 0x"++c++")"

--

-- instruction parsers

movParser :: Parser Operation
movParser = Mov <$>  (string "mov" >> many1 space >> registerParser) <*> (char ',' >> spaces >> anyValParser)

interruptParser :: Parser Operation
interruptParser = Interrupt <$> (string "int" >> many1 space >> onlyValParser)

incParser :: Parser Operation
incParser = Inc <$> (string "inc" >> many1 space >> registerParser)

decParser :: Parser Operation
decParser = Dec <$> (string "dec" >> many1 space >> registerParser)

jmpParser :: Parser Operation
jmpParser = Jmp <$> (string "jmp" >> many1 space >> many1 letterDigitParser)

cmpParser :: Parser Operation
cmpParser = Cmp <$>  (string "cmp" >> many1 space >> anyValParser) <*> (char ',' >> spaces >> anyValParser)

finalParser :: Parser Operation
finalParser = try movParser <|> try interruptParser <|> try incParser <|> try decParser <|> try cmpParser <|> try jmpParser

insToBin :: Operation -> String
insToBin (Mov a b) = "copying value from "++(valToBin $ b)++" to "++(valToBin $ a)
insToBin (Interrupt code) = "interrupting with code "++(valToBin $ code)
insToBin (Inc reg) = "incrementing "++(valToBin $ reg)++" value"
insToBin (Dec reg) = "decrementing "++(valToBin $ reg)++" value"
insToBin (Cmp a b) = "comparing "++(valToBin $ a)++" with "++(valToBin $ b)
insToBin (Jmp l) = "unconditional jump to "++l

--
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
 
