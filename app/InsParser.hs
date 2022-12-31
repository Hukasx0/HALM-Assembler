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

useMLMParser :: Parser Operation
useMLMParser = UseMLM <$> (string "use" >> many1 space >> many1 letter)

insToBin :: Operation -> MacroTable -> MLMacroTable -> [Word8]
insToBin (Mov a b) mT _= [176+(valToBin a mT)!!0]++(valToBin b mT)
insToBin (Interrupt code) mT _= [205]++[(valToBin code mT)!!0]
insToBin (Inc reg) mT _= [254]++[192+(valToBin reg mT)!!0]
insToBin (Dec reg) mT _= [254]++[200+(valToBin reg mT)!!0]
insToBin (Cmp (Register a) (Register b)) mT _ = [56] ++ [192+ (8* (valToBin (Register b) mT)!!0) +(valToBin (Register a) mT)!!0]
insToBin (Cmp (Register "al") b) mT _ = [60] ++ [(valToBin b mT)!!0]
insToBin (Cmp a b) mT _ = [128]++[248+(valToBin a mT)!!0]++[(valToBin b mT)!!0]
insToBin (Jmp "$") _ _= [235,254]
insToBin (Jmp _) _ _= [235,0]
insToBin (AdBy bytes) mT _= concat $ map (\b -> valToBin b mT) bytes
insToBin (UseMLM name) mT mlmtable = case  (lookup name mlmtable ) of
                        Just value -> concat $ (map (\operation -> insToBin operation mT mlmtable) value)
                        Nothing -> error $ "This macro doesn't exist!"
insToBin _ _ _= []
