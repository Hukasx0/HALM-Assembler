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

anyValParser :: Parser Value
anyValParser = try registerParser <|> try hexParser <|> try intParser <|> try charParser <|> try stringParser <|> mathParser

onlyValParser :: Parser Value
onlyValParser = try hexParser <|> try intParser <|> try charParser <|> try stringParser <|> mathParser

valToBin :: Value -> [Word8]
valToBin(Register a) |a=="al"=[0]
                     |a=="cl"=[1]
                     |a=="dl"=[2]
                     |a=="bl"=[3]
                     |a=="ah"=[4]
                     |a=="ch"=[5]
                     |a=="dh"=[6]
                     |a=="bh"=[7]

valToBin(Int b) = [(read b::Word8)]
valToBin(Hex c) = [(fromIntegral $ getHexFromStr c)]
valToBin(Ch d) = [(fromIntegral $ (fromEnum d))]
valToBin(Str e) = map (\x -> fromIntegral $ fromEnum x) e
valToBin(Math operator a b) = mathInterpreter operator a b

mathInterpreter :: String -> Value -> Value -> [Word8]
mathInterpreter "times" (Int a) b = concat $ replicate (read a::Int) (valToBin $ b)
mathInterpreter "+" a b = (+) <$> (valToBin $ a) <*> (valToBin $ b)
mathInterpreter "-" a b = (-) <$> (valToBin $ a) <*> (valToBin $ b)
mathInterpreter "*" a b = (*) <$> (valToBin $ a) <*> (valToBin $ b)
mathInterpreter "/" a b = (div) <$> (valToBin $ a) <*> (valToBin $ b)
mathInterpreter "++" a b = (valToBin $ a) ++ (valToBin $ b)
