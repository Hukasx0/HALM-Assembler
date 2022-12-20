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

finalParser :: Parser Operation
finalParser = try movParser <|> try interruptParser <|> try incParser <|> try decParser <|> try cmpParser <|> try jmpParser

insToBin :: Operation -> [Word8]
insToBin (Mov a b) = [176+(valToBin $ a)]++[(valToBin $ b)]
insToBin (Interrupt code) = [205]++[(valToBin $ code)]
insToBin (Inc reg) = [254]++[192+(valToBin $ reg)]
insToBin (Dec reg) = [254]++[200+(valToBin $ reg)]
insToBin (Cmp _ _) = [0,0]
insToBin (Jmp _) = [0,0]