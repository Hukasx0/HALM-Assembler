module ValParser where

import Text.Parsec
import Text.Parsec.String
import Data.Word

import Values

registerParser :: Parser Value
registerParser = Register <$> (string "al" <|> string "bl" <|> string "cl" <|> string "dl")

intParser :: Parser Value
intParser = Int <$> (many1 digit)

hexParser :: Parser Value
hexParser = Hex <$> (string "0x" >> many1 hexDigit)

charParser :: Parser Value
charParser = Ch <$> ( between (char '\'') (char '\'') (anyChar) )

stringParser :: Parser Value
stringParser = Str <$> (between (char '"') (char '"') (many (noneOf "\"")))

anyValParser :: Parser Value
anyValParser = try registerParser <|> try hexParser <|> try intParser <|> try charParser <|> stringParser

onlyValParser :: Parser Value
onlyValParser = try hexParser <|> try intParser <|> try charParser <|> stringParser

valToBin :: Value -> [Word8]
valToBin(Register a) |a=="al"=[0]
                     |a=="cl"=[1]
                     |a=="dl"=[2]
                     |a=="bl"=[3]

valToBin(Int b) = [(read b::Word8)]
valToBin(Hex c) = [(fromIntegral $ getHexFromStr c)]
valToBin(Ch d) = [(fromIntegral $ (fromEnum d))]
valToBin(Str e) = map (\x -> fromIntegral $ fromEnum x) e