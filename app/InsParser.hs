module InsParser where

import Text.Parsec
import Text.Parsec.String
import Data.Word

import Values
import ValParser

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

addByParser :: Parser Operation
addByParser = AdBy <$> (string "addBytes" >> spaces >> char '=' >> spaces >> (onlyValParser `sepBy` (char ',') ))

finalParser :: Parser Operation
finalParser = try movParser <|> try interruptParser <|> try incParser <|> try decParser <|> try cmpParser <|> try jmpParser <|> addByParser

insToBin :: Operation -> [Word8]
insToBin (Mov a b) = [176+(valToBin $ a)!!0]++(valToBin b)
insToBin (Interrupt code) = [205]++[(valToBin $ code)!!0]
insToBin (Inc reg) = [254]++[192+(valToBin $ reg)!!0]
insToBin (Dec reg) = [254]++[200+(valToBin $ reg)!!0]
insToBin (Cmp _ _) = [0,0]
insToBin (Jmp _) = [0,0]
insToBin (AdBy bytes) = concat $ map valToBin bytes
