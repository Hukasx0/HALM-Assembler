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
jmpParser = Jmp <$> (string "jmp" >> many1 space >> many1 (noneOf " \n\r"))

cmpParser :: Parser Operation
cmpParser = Cmp <$>  (string "cmp" >> many1 space >> anyValParser) <*> (char ',' >> spaces >> anyValParser)

addByParser :: Parser Operation
addByParser = AdBy <$> (string "addBytes" >> spaces >> char '=' >> spaces >> (onlyValParser `sepBy` (char ',') ))

insToBin :: Operation -> MacroTable -> [Word8]
insToBin (Mov a b) mT= [176+(valToBin a mT)!!0]++(valToBin b mT)
insToBin (Interrupt code) mT= [205]++[(valToBin code mT)!!0]
insToBin (Inc reg) mT= [254]++[192+(valToBin reg mT)!!0]
insToBin (Dec reg) mT= [254]++[200+(valToBin reg mT)!!0]
insToBin (Cmp _ _) mT= [0,0]
insToBin (Jmp "$") mT= [235,254]
insToBin (Jmp _) mT= [235,0]
insToBin (AdBy bytes) mT= concat $ map (\b -> valToBin b mT) bytes
insToBin _ _= []
