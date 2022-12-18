module Main where

--import System.Environment
--import Data.List
import Text.Parsec
import Text.Parsec.String
import Numeric (readHex)

type Label = String

data Value = Register String | Int String | Hex String
                deriving(Eq,Show)

data Operation = Mov Value Value | Interrupt Value | Inc Value | Dec Value | Cmp Value Value | Jmp Label
                deriving(Eq,Show)

letterDigitParser :: Parsec String () Char
letterDigitParser = oneOf ['0'..'9'] <|> oneOf ['a'..'z'] <|> oneOf ['A'..'Z']

getHexFromStr :: String -> Int
getHexFromStr str = fst $ head $ readHex str

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

valToBin :: Value -> Int
valToBin(Register a) |a=="al"=0
                     |a=="cl"=1
                     |a=="dl"=2
                     |a=="bl"=3

valToBin(Int b) = read b::Int
valToBin(Hex c) = getHexFromStr c

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

insToBin :: Operation -> [Int]
insToBin (Mov a b) = [176+(valToBin $ a)]++[(valToBin $ b)]
insToBin (Interrupt code) = [205]++[(valToBin $ code)]
insToBin (Inc reg) = [254]++[192+(valToBin $ reg)]
insToBin (Dec reg) = [254]++[200+(valToBin $ reg)]
insToBin (Cmp _ _) = [0,0]
insToBin (Jmp _) = [0,0]

--
codeToIns :: [Operation] -> [[Int]]
codeToIns code = map insToBin code

virtualFile :: String
virtualFile = "mov al,123\nmov bl,0xA\nmov dl,al\ninc al\ndec dl\ncmp al,123\nint 0x10\njmp test\n"

main :: IO ()
main = do putStrLn $ ("input:\n"++virtualFile) 
          let parsed = parse (spaces >> many (finalParser <* many1 space) <* eof) "virtual" virtualFile
          putStrLn $ "output:"
          case parsed of
            Left err -> print err
            Right corr -> print (codeToIns $ corr)
 
