module ValParser where

import Text.Parsec
import Text.Parsec.String
import Data.List(sort)
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

reverseParser :: Parser Value
reverseParser = Rev <$> (string "reverse" >> many1 space >> (try anyValParser <|> fileConParser))

revManyParser :: Parser Value
revManyParser = RevMany <$> (string "reverseMany" >> many1 space >> (anyValParser `sepBy` (char ',') ))

sortParser :: Parser Value
sortParser = Sort <$> (string "sort" >> many1 space >> (try anyValParser <|> fileConParser))

sortManyParser :: Parser Value
sortManyParser = SortMany <$> (string "sortMany" >> many1 space >> (anyValParser `sepBy` (char ',') ))

filterParser :: Parser Value
filterParser = Filter <$> (string "filterx" >> spaces >> mathParser) <*> (many1 space >> ((try anyValParser <|> fileConParser) `sepBy` (char ',') ))

mapParser :: Parser Value
mapParser = Filter <$> (string "mapx" >> spaces >> mathParser) <*> (many1 space >> (anyValParser `sepBy` (char ',') ))

countParser :: Parser Value
countParser = Count <$> (string "count" >> spaces >> (try anyValParser <|> fileConParser))

paramParser :: Parser Value
paramParser = Parameter <$> (char '\'' >> many1 letter)

pureStringParser :: Parser String
pureStringParser = between (char '"') (char '"') (many (noneOf "\""))

mathParser :: Parser Value
mathParser = Math <$> (char '(' >> spaces >> many1 (noneOf " \n\r")) <*> (many1 space >> onlyValParser) <*> (many1 space >> onlyValParser <* spaces <*  char ')')

macroParser :: Parser Value
macroParser = UseM <$> (char '\\' >> many1 letter)

fileConParser :: Parser Value
fileConParser = FileCon <$> (char '(' >> spaces >> string "readF" >> spaces >> char '=' >> spaces >> pureStringParser <* spaces <* char ')')

anyValParser :: Parser Value
anyValParser =try filterParser<|>try paramParser <|>try countParser <|> try revManyParser <|> try reverseParser <|>try sortManyParser <|> try sortParser<|>try registerParser <|> try hexParser <|> try octParser <|> try binParser <|> try intParser <|> try charParser <|> try stringParser <|> try mathParser <|>try macroParser <|>try pointerParser <|>try derefParser

onlyValParser :: Parser Value
onlyValParser =try filterParser<|>try paramParser <|>try countParser <|> try revManyParser <|> try reverseParser <|>try sortManyParser <|> try sortParser<|>try hexParser <|> try intParser <|> try octParser <|> try binParser <|> try charParser <|> try stringParser <|> try mathParser <|>try macroParser <|>try pointerParser <|>try derefParser

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
valToBin(Rev a) mT = reverse $ (valToBin a mT)
valToBin(RevMany a) mT =reverse $ concat $ map (\v -> valToBin v mT) a
valToBin(Sort a) mT = sort $ (valToBin a mT)
valToBin(SortMany a) mT =sort $ concat $ map (\v -> valToBin v mT) a
valToBin(Count a) mT = intToWord8List $ length $ (valToBin a mT)
valToBin(Ret _) _ = [0]
valToBin(Retr _) _ = [0]
valToBin(Filter (Math op (Parameter "x") b) vals) mT = concat $ map (\f -> valToBin f mT) (filter (\v -> word8ToBoolean $ mathInterpreter op v b mT) (concat $ map (\val -> map (\w8 ->Int (show $ word8ListToInt $ [w8])) (valToBin val mT)) vals))
valToBin(Filter (Math op b (Parameter "x")) vals) mT = concat $ map (\f -> valToBin f mT) (filter (\v -> word8ToBoolean $ mathInterpreter op b v mT) (concat $ map (\val -> map (\w8 ->Int (show $ word8ListToInt $ [w8])) (valToBin val mT)) vals))
valToBin(Math operator a b) mT = mathInterpreter operator a b mT
valToBin(UseM macro) mT = case  (lookup macro mT ) of
                        Just value -> valToBin value mT
                        Nothing -> error $ "This macro doesn't exist!"

mathInterpreter :: String -> Value -> Value -> MacroTable -> [Word8]
mathInterpreter "times" (Math a c d) b mT= concat $ replicate (word8ListToInt $ (valToBin (Math a c d) mT)) (valToBin b mT)
mathInterpreter "times" a b mT= concat $ replicate (word8ListToInt $ (valToBin a mT)) (valToBin b mT)
mathInterpreter "+" a b mT= valToBin (Int <$> show  $ ( (word8ListToInt $ valToBin a mT) + (word8ListToInt $ valToBin b mT) ) ) mT
mathInterpreter "%" a b mT= valToBin (Int <$> show  $ ( (word8ListToInt $ valToBin a mT) `mod` (word8ListToInt $ valToBin b mT) ) ) mT
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
mathInterpreter ">" a b mT = booleanToWord8List $ ((word8ListToInt $ valToBin a mT)>(word8ListToInt $ valToBin b mT))
mathInterpreter ">=" a b mT = booleanToWord8List $ ((word8ListToInt $ valToBin a mT)>=(word8ListToInt $ valToBin b mT))
mathInterpreter "<" a b mT = booleanToWord8List $ ((word8ListToInt $ valToBin a mT)<(word8ListToInt $ valToBin b mT))
mathInterpreter "<=" a b mT = booleanToWord8List $ ((word8ListToInt $ valToBin a mT)<=(word8ListToInt $ valToBin b mT))
mathInterpreter _ _ _ _ = error $ "Not known operation in brackets (operation value1 value2)"


getLabelAddr :: String -> LabelTable -> [Word8]
getLabelAddr lblName lT = case  (lookup lblName lT ) of
                        Just value -> intToWord8List $ value
                        Nothing -> [0] --error $ "This label doesn't exist!"

valOrParam :: Value -> [(String,Value)] -> MacroTable -> [Word8]
valOrParam (Math a (Parameter b) c) mp mt= valOrParam (Math a (Str (word8ListToString $ (case (lookup b mp) of
                                Just value -> (valToBin value mt)
                                Nothing -> error $ "Parameter value not set!"))) c) mp mt
valOrParam (Math a c (Parameter b)) mp mt= valOrParam (Math a c (Str (word8ListToString $ (case (lookup b mp) of
                                Just value -> (valToBin value mt)
                                Nothing -> error $ "Parameter value not set!")))) mp mt
valOrParam (Parameter a) mp mt=  case (lookup a mp) of
                                Just value -> (valToBin value mt)
                                Nothing -> error $ "Parameter value not set!"
valOrParam (Rev a) mp mt =  reverse $ (valOrParam a mp mt)
valOrParam (Sort a) mp mt =  sort $ (valOrParam a mp mt)
valOrParam a _ _= (valToBin a [])
