module ValParser where

import Text.Parsec
import Text.Parsec.String
import Data.List(sort)
import qualified Data.ByteString.Lazy as B
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

arrayParser :: Parser Value
arrayParser = Array <$> (char '[' >> spaces >> (anyValParser `sepBy` (char ',') ) <* spaces <* char ']')

darrayParser :: Parser Value
darrayParser = Darr <$> (char '[' >> spaces >> anyValParser) <*> (spaces >> string ".." >> spaces >> anyValParser <* spaces <* char ']')

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
filterParser = Filter <$> (string "filterx" >> spaces >> mathParser) <*> (many1 space >> ((try anyValParser <|>try fileConParser <|> promptParser) `sepBy` (char ',') ))

mapParser :: Parser Value
mapParser = Map <$> (string "mapx" >> spaces >> mathParser) <*> (many1 space >> ((try anyValParser <|>try fileConParser <|> promptParser) `sepBy` (char ',') ))

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

promptParser :: Parser Value
promptParser = Prompt <$> (char '(' >> spaces >>string "prompt" >> spaces >> char '='  >> spaces >> pureStringParser <* spaces <* char ')')

anyValParser :: Parser Value
anyValParser =try filterParser<|>try mapParser<|>try charParser<|>try paramParser <|>try countParser <|>try darrayParser<|>try arrayParser<|>try revManyParser <|> try reverseParser <|>try sortManyParser <|> try sortParser<|>try registerParser <|> try hexParser <|> try octParser <|> try binParser <|> try intParser <|> try charParser <|> try stringParser <|> try mathParser <|>try macroParser <|>try pointerParser <|>try derefParser

onlyValParser :: Parser Value
onlyValParser =try filterParser<|>try mapParser<|>try charParser <|>try paramParser <|>try countParser <|>try darrayParser<|>try arrayParser<|>try revManyParser <|> try reverseParser <|>try sortManyParser <|> try sortParser<|>try hexParser <|> try intParser <|> try octParser <|> try binParser <|> try stringParser <|> try mathParser <|>try macroParser <|>try pointerParser <|>try derefParser

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
valToBin(Array e) mT = concat $ map (\x -> valToBin x mT) e
valToBin(Darr x y) mT = concat $ map (intToWord8List) [(word8ListToInt $ valToBin x mT)..(word8ListToInt $ valToBin y mT)]
valToBin(Rev a) mT = reverse $ (valToBin a mT)
valToBin(RevMany a) mT =reverse $ concat $ map (\v -> valToBin v mT) a
valToBin(Sort a) mT = sort $ (valToBin a mT)
valToBin(SortMany a) mT =sort $ concat $ map (\v -> valToBin v mT) a
valToBin(Count a) mT = intToWord8List $ length $ (valToBin a mT)
valToBin(Ret _) _ = [0]
valToBin(Retr _) _ = [0]
--valToBin(Filter (Math op (Parameter "x") b) vals) mT = concat $ map (\f -> valToBin f mT) (filter (\v -> word8ToBoolean $ mathInterpreter op v b mT) (concat $ map (\val -> map (\w8 ->Int (show $ word8ListToInt $ [w8])) (valToBin val mT)) vals))
--valToBin(Filter (Math op b (Parameter "x")) vals) mT = concat $ map (\f -> valToBin f mT) (filter (\v -> word8ToBoolean $ mathInterpreter op b v mT) (concat $ map (\val -> map (\w8 ->Int (show $ word8ListToInt $ [w8])) (valToBin val mT)) vals))
valToBin(Filter a vals) mT = concat $ map (\f -> valToBin f mT) (filter (\v -> word8ToBoolean $ valToBin (getParamX a v) mT) (concat $ map (\val -> map (\w8 ->Int (show $ word8ListToInt $ [w8])) (valToBin val mT)) vals))
--valToBin(Map (Math op (Parameter "x") b) vals) mT = concat $ (map (\v -> mathInterpreter op v b mT) (concat $ map (\val -> map (\w8 ->Int (show $ word8ListToInt $ [w8])) (valToBin val mT)) vals))
--valToBin(Map (Math op b (Parameter "x")) vals) mT = concat $ (map (\v -> mathInterpreter op b v mT) (concat $ map (\val -> map (\w8 ->Int (show $ word8ListToInt $ [w8])) (valToBin val mT)) vals))
valToBin(Map a vals) mT = concat $ (map (\v -> valToBin (getParamX a v) mT) (concat $ map (\val -> map (\w8 ->Int (show $ word8ListToInt $ [w8])) (valToBin val mT)) vals))
valToBin(Math operator a b) mT = mathInterpreter operator a b mT
valToBin(UseM macro) mT = case  (lookup macro mT ) of
                        Just value -> valToBin value mT
                        Nothing -> error $ "This macro doesn't exist!"

getParamX :: Value -> Value -> Value
getParamX(Math op (Parameter "x") (Parameter "x")) xVal =(Math op xVal xVal) 
getParamX(Math op (Parameter "x") b) xVal =(Math op xVal b) 
getParamX(Math op b (Parameter "x")) xVal =(Math op b xVal) 
getParamX(Math op (Math a b c) (Math d e f)) xVal= (Math op (getParamX (Math a b c) xVal) (getParamX (Math d e f) xVal))
getParamX(Math op (Math a b c) d) xVal= (Math op (getParamX (Math a b c) xVal) d)
getParamX(Math op d (Math a b c)) xVal= (Math op d (getParamX (Math a b c) xVal))
getParamX(Filter a b) _=(Filter a b)
getParamX(Map a b) _=(Map a b)
getParamX a _ = a

getHLVal :: Value -> MacroTable -> IO [Word8]
getHLVal(Rev a) mT =do
                    rv <- (getHLVal a mT)
                    pure $ (reverse $ rv)
getHLVal(Sort a) mT =do
                    rv <- (getHLVal a mT)
                    pure $ (sort $ rv)
getHLVal(Count a) mT =do
                    rv <- (getHLVal a mT)
                    pure $ (intToWord8List $ length $ rv)
getHLVal(Filter a b) mT=do
                        rv <- mapM (\v -> getHLVal v mT) b
                        pure $ (valToBin (Filter a (map (\v -> (Str (word8ListToString $ v)) ) rv)) mT)
getHLVal(Map a b) mT=do
                        rv <- mapM (\v -> getHLVal v mT) b
                        pure $ (valToBin (Map a (map (\v -> (Str (word8ListToString $ v)) ) rv)) mT)
getHLVal(Prompt msg) mT = (putStrLn $ msg) >>= \_ -> getLine >>= \inp -> pure $ (valToBin (Str inp) mT)
getHLVal(FileCon filename) mT = (B.readFile $ filename) >>= \fileData -> pure $ (valToBin (Str (word8ListToString $ B.unpack $ fileData)) mT)
getHLVal a mT = pure $ valToBin a mT

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
