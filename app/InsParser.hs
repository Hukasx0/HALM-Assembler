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
jmpParser = Jmp <$> (string "jmp" >> many1 space >> many1 (noneOf " \n\r")) <*> (pure 0)

jeParser :: Parser Operation
jeParser = Je <$> (string "je" >> many1 space >> many1 (noneOf " \n\r")) <*> (pure 0)

jneParser :: Parser Operation
jneParser = Jne <$> (string "jne" >> many1 space >> many1 (noneOf " \n\r")) <*> (pure 0)

jgParser :: Parser Operation
jgParser = Jg <$> (string "jg" >> many1 space >> many1 (noneOf " \n\r")) <*> (pure 0)

jgeParser :: Parser Operation
jgeParser = Jge <$> (string "jge" >> many1 space >> many1 (noneOf " \n\r")) <*> (pure 0)

jlParser :: Parser Operation
jlParser = Jl <$> (string "jl" >> many1 space >> many1 (noneOf " \n\r")) <*> (pure 0)

jleParser :: Parser Operation
jleParser = Jle <$> (string "jle" >> many1 space >> many1 (noneOf " \n\r")) <*> (pure 0)

addParser :: Parser Operation
addParser = Add <$> (string "add" >> many1 space >> registerParser) <*> (char ',' >> spaces >> anyValParser)

subParser :: Parser Operation
subParser = Sub <$> (string "sub" >> many1 space >> registerParser) <*> (char ',' >> spaces >> anyValParser)

negParser :: Parser Operation
negParser = Neg <$> (string "neg" >> many1 space >> registerParser)

xorParser :: Parser Operation
xorParser = Xor <$> (string "xor" >> many1 space >> registerParser) <*> (char ',' >> spaces >> anyValParser)

cmpParser :: Parser Operation
cmpParser = Cmp <$>  (string "cmp" >> many1 space >> anyValParser) <*> (char ',' >> spaces >> anyValParser)

defLabelParser :: Parser Operation
defLabelParser = DefLabel <$> (string "label" >> many1 space >> many1 letter)

addByParser :: Parser Operation
addByParser = AdBy <$> (string "addBytes" >> spaces >> char '=' >> spaces >> (onlyValParser `sepBy` (char ',') ))

useMLMParser :: Parser Operation
useMLMParser = UseMLM <$> (string "use" >> many1 space >> many1 letter)

fillBytesParser :: Parser Operation
fillBytesParser = FillB <$> (string "fillBytes" >> many1 space >> onlyValParser) <*> (many1 space >> onlyValParser)

getCurrBytes :: Operation -> MacroTable -> MLMacroTable -> [(String,Int)]
getCurrBytes (FillB a _) _ _ = [("fillB",0),("code",(length $ replicate (word8ListToInt $ (valToBin a [("",(Int "0"))])) 0))]
getCurrBytes (DefLabel _) _ _ = [("defL",0)]
getCurrBytes (Jmp _ _) _ _ = [("jump",0),("code",2)]
getCurrBytes (Je _ _) _ _ = [("jump",0),("code",2)]
getCurrBytes (Jne _ _) _ _ = [("jump",0),("code",2)]
getCurrBytes (Jg _ _) _ _ = [("jump",0),("code",2)]
getCurrBytes (Jle _ _) _ _ = [("jump",0),("code",2)]
getCurrBytes (Jge _ _) _ _ = [("jump",0),("code",2)]
getCurrBytes (Jl _ _) _ _ = [("jump",0),("code",2)]
getCurrBytes op mt mlm = [("code",(length $ (insToBin op mt mlm [("",0)])))]

sumList :: [(String, Int)] -> [Int]
sumList xs = snd $ foldl f (0, []) xs
  where
    f (acc, res) ("code", x) = (acc + x, res)
    f (acc, res) (_, _) = (acc, acc:res)

byteFilter :: Operation -> [Operation]
byteFilter (FillB a b) = [(FillB a b)]
byteFilter (Jmp a b) = [(Jmp a b)]
byteFilter (Je a b) = [(Je a b)]
byteFilter (Jne a b) = [(Jne a b)]
byteFilter (Jg a b) = [(Jg a b)]
byteFilter (Jle a b) = [(Jle a b)]
byteFilter (Jge a b) = [(Jge a b)]
byteFilter (Jl a b) = [(Jl a b)]
byteFilter (DefLabel dLabel) = [(DefLabel dLabel)]
byteFilter _ = []

getLabelTable :: (Operation,Int) -> [(String,Int)]
getLabelTable ((DefLabel dLabel),addr) = [(dLabel,addr)]
getLabelTable _ = []

fillBFilter :: (Operation,Int) -> Operation
fillBFilter ((FillB times byte),bytesCtr) = (AdBy [(Math "times" (Math "-" (times) (Int (show $ bytesCtr) ) ) (byte) )])
fillBFilter ((Jmp lbl _),bytesCtr) = ((Jmp lbl bytesCtr))
fillBFilter ((Je lbl _),bytesCtr) = ((Je lbl bytesCtr))
fillBFilter ((Jne lbl _),bytesCtr) = ((Jne lbl bytesCtr))
fillBFilter ((Jg lbl _),bytesCtr) = ((Jg lbl bytesCtr))
fillBFilter ((Jle lbl _),bytesCtr) = ((Jle lbl bytesCtr))
fillBFilter ((Jge lbl _),bytesCtr) = ((Jge lbl bytesCtr))
fillBFilter ((Jl lbl _),bytesCtr) = ((Jl lbl bytesCtr))
fillBFilter (rest,_) = rest

clearData :: Operation -> [Operation]
clearData (DefLabel _) = []
clearData rest = [rest]

replaceValues :: [Operation] -> [Operation] -> [Operation]
replaceValues ops news = go ops news
  where
    go [] _ = []
    go (op:ops) news =
      if isJmp op || isFillB op
      then head news : go ops (tail news)
      else op : go ops news
    isJmp (Jmp _ _) = True
    isJmp (Je _ _)  = True
    isJmp (Jne _ _) = True
    isJmp (Jg _ _)  = True
    isJmp (Jle _ _) = True
    isJmp (Jge _ _) = True
    isJmp (Jl _ _) = True
    isJmp _          = False
    isFillB (FillB _ _) = True
    isFillB _            = False

insToBin :: Operation -> MacroTable -> MLMacroTable -> LabelTable -> [Word8]
insToBin (Mov a b) mT _ _= [176+(valToBin a mT)!!0]++(valToBin b mT)
insToBin (Interrupt code) mT _ _= [205]++[(valToBin code mT)!!0]
insToBin (Inc reg) mT _ _= [254]++[192+(valToBin reg mT)!!0]
insToBin (Dec reg) mT _ _= [254]++[200+(valToBin reg mT)!!0]
insToBin (Cmp (Register a) (Register b)) mT _ _= [56] ++ [192+ (8* (valToBin (Register b) mT)!!0) +(valToBin (Register a) mT)!!0]
insToBin (Cmp (Register "al") b) mT _ _= [60] ++ [(valToBin b mT)!!0]
insToBin (Cmp a b) mT _ _= [128]++[248+(valToBin a mT)!!0]++[(valToBin b mT)!!0]
insToBin (Jmp "$" _) _ _ _= [235,254]
insToBin (Jmp lbl addr) _ _ labelTbl= case  (lookup lbl labelTbl ) of
                        Just value -> if (value>addr) then [235]++(intToWord8List $ (value-addr-2))
                                      else [235]++[254-(intToWord8List $ addr)!!0]
                        Nothing -> error $ "This label doesn't exist!"
insToBin (Je lbl addr) _ _ labelTbl= case  (lookup lbl labelTbl ) of
                        Just value -> if (value>addr) then [116]++(intToWord8List $ (value-addr-2))
                                      else [116]++[254-(intToWord8List $ addr)!!0]
                        Nothing -> error $ "This label doesn't exist!"
insToBin (Jne lbl addr) _ _ labelTbl= case  (lookup lbl labelTbl ) of
                        Just value -> if (value>addr) then [117]++(intToWord8List $ (value-addr-2))
                                      else [117]++[254-(intToWord8List $ addr)!!0]
                        Nothing -> error $ "This label doesn't exist!"
insToBin (Jg lbl addr) _ _ labelTbl= case  (lookup lbl labelTbl ) of
                        Just value -> if (value>addr) then [127]++(intToWord8List $ (value-addr-2))
                                      else [127]++[254-(intToWord8List $ addr)!!0]
                        Nothing -> error $ "This label doesn't exist!"
insToBin (Jle lbl addr) _ _ labelTbl= case  (lookup lbl labelTbl ) of
                        Just value -> if (value>addr) then [126]++(intToWord8List $ (value-addr-2))
                                      else [126]++[254-(intToWord8List $ addr)!!0]
                        Nothing -> error $ "This label doesn't exist!"
insToBin (Jge lbl addr) _ _ labelTbl= case  (lookup lbl labelTbl ) of
                        Just value -> if (value>addr) then [125]++(intToWord8List $ (value-addr-2))
                                      else [125]++[254-(intToWord8List $ addr)!!0]
                        Nothing -> error $ "This label doesn't exist!"
insToBin (Jl lbl addr) _ _ labelTbl= case  (lookup lbl labelTbl ) of
                        Just value -> if (value>addr) then [124]++(intToWord8List $ (value-addr-2))
                                      else [124]++[254-(intToWord8List $ addr)!!0]
                        Nothing -> error $ "This label doesn't exist!"
insToBin (Add (Register a) (Register b)) mT _ _= [00] ++ [192+(8* (valToBin (Register b) mT)!!0)+(valToBin (Register a) mT)!!0]
insToBin (Add (Register "al") b) mT _ _= [04]++(valToBin b mT)
insToBin (Add a b) mT _ _= [128]++[192+(valToBin a mT)!!0]++(valToBin b mT)
insToBin (Sub (Register a) (Register b)) mT _ _= [40]++[192+(8* (valToBin (Register b) mT)!!0)+(valToBin (Register a) mT)!!0]
insToBin (Sub (Register "al") b) mT _ _= [44]++(valToBin b mT)
insToBin (Sub a b) mT _ _= [128]++[232+(valToBin a mT)!!0]++(valToBin b mT)
insToBin (Neg a) mT _ _=[246]++[216+(valToBin a mT)!!0]
insToBin (Xor (Register a) (Register b)) mT _ _= [48]++[192+(8* (valToBin (Register b) mT)!!0)+(valToBin (Register a) mT)!!0]
insToBin (Xor (Register "al") b) mT _ _=[52]++(valToBin b mT)
insToBin (Xor a b) mT _ _= [128]++[240+(valToBin a mT)!!0]++(valToBin b mT)
insToBin (AdBy bytes) mT _ _= concat $ map (\b -> valToBin b mT) bytes
insToBin (UseMLM name) mT mlmtable labelTbl= case  (lookup name mlmtable ) of
                        Just value -> concat $ (map (\operation -> insToBin operation mT mlmtable labelTbl) value)
                        Nothing -> error $ "This macro doesn't exist!"
insToBin _ _ _ _= []
