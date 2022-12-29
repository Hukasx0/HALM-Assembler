module ValParser where

import Text.Parsec
import Text.Parsec.String
import Data.Word

import Values

registerParser :: Parser Value
registerParser = Register <$> (try (string "al") <|>try (string "bl") <|>try (string "cl") <|>try (string "dl")
                               <|> string "ah" <|> string "bh" <|> string "ch" <|> string "dh")

intParser :: Parser Value
intParser = Int <$> (many1 digit)

hexParser :: Parser Value
hexParser = Hex <$> (string "0x" >> many1 hexDigit)

charParser :: Parser Value
charParser = Ch <$> ( between (char '\'') (char '\'') (anyChar) )

stringParser :: Parser Value
stringParser = Str <$> (between (char '"') (char '"') (many (noneOf "\"")))

pureStringParser :: Parser String
pureStringParser = between (char '"') (char '"') (many (noneOf "\""))

mathParser :: Parser Value
mathParser = Math <$> (char '(' >> spaces >> many1 (noneOf " \n\r")) <*> (many1 space >> onlyValParser) <*> (many1 space >> onlyValParser <* spaces <*  char ')')

macroParser :: Parser Value
macroParser = UseM <$> (char '\\' >> many1 letter)

anyValParser :: Parser Value
anyValParser = try registerParser <|> try hexParser <|> try intParser <|> try charParser <|> try stringParser <|> try mathParser <|> macroParser 

onlyValParser :: Parser Value
onlyValParser = try hexParser <|> try intParser <|> try charParser <|> try stringParser <|> try mathParser <|> macroParser

valToBin :: Value -> MacroTable -> [Word8]
valToBin(Register a) _ |a=="al"=[0]
                       |a=="cl"=[1]
                       |a=="dl"=[2]
                       |a=="bl"=[3]
                       |a=="ah"=[4]
                       |a=="ch"=[5]
                       |a=="dh"=[6]
                       |a=="bh"=[7]

valToBin(Int x) _ = intToWord8List (read $ x)

valToBin(Hex c) _ = [(fromIntegral $ getHexFromStr c)]
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
