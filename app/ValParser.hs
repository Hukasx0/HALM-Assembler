module ValParser where

import Text.Parsec
import Text.Parsec.String
import Data.Word

import Values

registerParser8 :: Parser Value
registerParser8 = Register8 <$> (try (string "al") <|>try (string "bl") <|>try (string "cl") <|>try (string "dl")
                               <|> try (string "ah") <|> try (string "bh") <|> try (string "ch") <|> try (string "dh"))

registerParser16 :: Parser Value
registerParser16 = Register16 <$> (string "ax" <|> string "bx" <|> string "cx" <|> string "dx")

intParser :: Parser Value
intParser = Int <$> (many1 digit)

hexParser :: Parser Value
hexParser = Hex <$> (string "0x" >> many1 hexDigit)

octParser :: Parser Value
octParser = Oct <$> (string "0o" >> many1 octDigit)

binParser :: Parser Value
binParser = Bin <$> (string "0b" >> (many1 $ oneOf "01"))

charParser :: Parser Value
charParser = Ch <$> ( between (char '\'') (char '\'') (anyChar) )

stringParser :: Parser Value
stringParser = Str <$> (between (char '"') (char '"') (many (noneOf "\"")))

pointerParser :: Parser Value
pointerParser = Pointer <$> (many1 letter)

derefParser :: Parser Value
derefParser = Deref <$> (between (char '[') (char ']') (many1 letter))

pureStringParser :: Parser String
pureStringParser = between (char '"') (char '"') (many (noneOf "\""))

mathParser :: Parser Value
mathParser = Math <$> (char '(' >> spaces >> many1 (noneOf " \n\r")) <*> (many1 space >> onlyValParser) <*> (many1 space >> onlyValParser <* spaces <*  char ')')

macroParser :: Parser Value
macroParser = UseM <$> (char '\\' >> many1 letter)

anyValParser :: Parser Value
anyValParser = try registerParser <|> try hexParser <|> try octParser <|> try binParser <|> try intParser <|> try charParser <|> try stringParser <|> try mathParser <|>try macroParser <|>try pointerParser <|> derefParser 

onlyValParser :: Parser Value
onlyValParser = try hexParser <|> try intParser <|> try octParser <|> try binParser <|> try charParser <|> try stringParser <|> try mathParser <|>try macroParser <|>try pointerParser <|> derefParser

registerParser :: Parser Value
registerParser = try registerParser8 <|> registerParser16

valToBin :: Value -> MacroTable -> [Word8]
valToBin(Register8 a) _ |a=="al"=[0]
                       |a=="cl"=[1]
                       |a=="dl"=[2]
                       |a=="bl"=[3]
                       |a=="ah"=[4]
                       |a=="ch"=[5]
                       |a=="dh"=[6]
                       |a=="bh"=[7]
valToBin(Register16 a) _ |a=="ax"=[8]
                       |a=="cx"=[9]
                       |a=="dx"=[10]
                       |a=="bx"=[11]

valToBin(Int x) _ = intToWord8List (read $ x)
valToBin(Hex c) _ = intToWord8List $ getHexFromStr $ c
valToBin(Oct x) _= intToWord8List $ getOctFromStr x 
valToBin(Bin x) _ =  intToWord8List $ getBinFromStr $ x
valToBin(Ch d) _ = [(fromIntegral $ (fromEnum d))]
valToBin(Str e) _ = map (\x -> fromIntegral $ fromEnum x) e
valToBin(Math operator a b) mT = mathInterpreter operator a b mT
valToBin(UseM macro) mT = case  (lookup macro mT ) of
                        Just value -> valToBin value mT
                        Nothing -> error $ "This macro doesn't exist!"

mathInterpreter :: String -> Value -> Value -> MacroTable -> [Word8]
mathInterpreter "times" (Math a c d) b mT= concat $ replicate (word8ListToInt $ (valToBin (Math a c d) mT)) (valToBin b mT)
mathInterpreter "times" a b mT= concat $ replicate (word8ListToInt $ (valToBin a mT)) (valToBin b mT)
mathInterpreter "+" a b mT= valToBin (Int <$> show  $ ( (word8ListToInt $ valToBin a mT) + (word8ListToInt $ valToBin b mT) ) ) mT
mathInterpreter "-" a b mT= valToBin (Int <$> show $ ( (word8ListToInt $ valToBin a mT) - (word8ListToInt $ valToBin b mT) )) mT
mathInterpreter "*" a b mT= valToBin (Int <$> show $ ( (word8ListToInt $ valToBin a mT) * (word8ListToInt $ valToBin b mT) )) mT
mathInterpreter "/" a b mT= valToBin (Int <$> show $  ( (word8ListToInt $ valToBin a mT) `div` (word8ListToInt $ valToBin b mT) )) mT
mathInterpreter "++" a b mT= (valToBin a mT) ++ (valToBin b mT)
mathInterpreter "==" a b mT |(valToBin a mT)==(valToBin b mT)=[1]
                            |otherwise=[0]
mathInterpreter "!=" a b mT |(valToBin a mT)/=(valToBin b mT)=[1]
                            |otherwise=[0]
mathInterpreter "and" a b mT |((word8ListToInt $ valToBin a mT)==1 && (word8ListToInt $ valToBin b mT)==1)=[1]
                            |otherwise=[0]
mathInterpreter "&&" a b mT |((word8ListToInt $ valToBin a mT)==1 && (word8ListToInt $ valToBin b mT)==1)=[1]
                            |otherwise=[0]
mathInterpreter "or" a b mT |((word8ListToInt $ valToBin a mT)==1 || (word8ListToInt $ valToBin b mT)==1)=[1]
                            |otherwise=[0]
mathInterpreter "||" a b mT |((word8ListToInt $ valToBin a mT)==1 || (word8ListToInt $ valToBin b mT)==1)=[1]
                            |otherwise=[0]
mathInterpreter _ _ _ _ = error $ "Not known operation in brackets (operation value1 value2)"

getLabelAddr :: String -> LabelTable -> [Word8]
getLabelAddr lblName lT = case  (lookup lblName lT ) of
                        Just value -> intToWord8List $ value
                        Nothing -> [0] --error $ "This label doesn't exist!"
