{-# LANGUAGE CPP #-}
module HLParser where

import Text.Parsec
import Text.Parsec.String
import System.Exit
import System.Process
import System.FilePath
import Control.Monad(void, replicateM_)
import qualified Data.ByteString.Lazy as B
import Data.List
import Values
import ValParser
import InsParser

isWindows :: Int
#if defined(WINDOWS)
isWindows = 1
#else
isWindows = 0
#endif

isUnix :: Int
#if defined(UNIX)
isUnix = 1
#else
isUnix = 0
#endif

doShParser :: Parser Operation
doShParser = DoSh <$> (string "doSh" >> spaces >> char '=' >> spaces >> pureStringParser)

dispParser :: Parser Operation
dispParser = Disp <$> (string "disp" >> many1 space >> anyValParser)

dispAParser :: Parser Operation
dispAParser = DispA <$> (string "Disp" >> many1 space >> anyValParser)

showVParser :: Parser Operation
showVParser = ShowV <$> (string "show" >> many1 space >> (try (string "str") <|> try (string "chars") <|>try (string "intArr") <|> try (string "int")<|> try (string "hexArr")
                    <|> try (string "hex") <|>try (string "octArr") <|> try (string "oct") <|> try (string "binArr") <|> try (string "bin") <|> try (string "bool") 
                    )) <*> (many1 space >> (hlAnyValParser `sepBy` (char ',' <* spaces) ))

hlAnyValParser :: Parser Value
hlAnyValParser = try anyValParser <|>try fileConParser <|> promptParser

lineCommentParser :: Parser Operation
lineCommentParser = Comment <$> (string "--" >> many (noneOf "\n"))

commentParser :: Parser Operation
commentParser = Comment <$> ( between (string "{-") (string "-}") (many (noneOf "-}")) )

defMacroParser :: Parser Operation
defMacroParser = DefM <$> (string "def" >> many1 space >> many1 letter) <*> (spaces >> char '=' >> spaces >> onlyValParser)

includeParser :: Parser Operation
includeParser = Incl <$> (string "include" >> many1 space >> many1 (noneOf " \n\r"))

shadowParser :: Parser Operation
shadowParser = Shadow <$> (string "shadow" >> many1 space >> string "def" >> spaces >> many1 letter)

defAsParser :: Parser Operation
defAsParser = DefAs <$> (string "def" >> many1 space >> string "as" >> many1 space >> many1 letter ) <*> (many1 space >> many1 letter) <*> (spaces >> char '=' >> spaces >> char '{' >> spaces >> many1 insParser <* spaces <* char '}')

useAsParser :: Parser Operation
useAsParser = UseAs <$> (many1 letter) <*> (spaces >> string "->" >> spaces >> many1 letter)

foreachParser :: Parser Operation
foreachParser = Foreach <$> (string "foreach" >> many1 space >> many1 letter) <*> (many1 space >> anyValParser `sepBy` (char ',') ) <*>  (many1 space >> char '{' >> spaces >>many1 insParser <* char '}')

retParser :: Parser Value
retParser = Ret <$> (char '(' >> spaces >> retInsParser <* spaces <* char ')')

retrParser :: Parser Value
retrParser = Retr <$> (string "!(" >> spaces >> retInsParser <* spaces <* char ')')

insParser :: Parser Operation
insParser = (try movParser <* spaces) <|> (try interruptParser <* spaces) <|> (try incParser <* spaces) <|> (try decParser <* spaces) 
              <|> (try cmpParser <* spaces) <|> (try jmpParser <* spaces) <|> (try jeParser <* spaces) <|> (try jneParser <* spaces)
              <|> (try jgParser <* spaces) <|> (try jgeParser <* spaces) <|> (try jlParser <* spaces) <|> (try jleParser <* spaces)
              <|> (try addByParser <* spaces) <|> (try doShParser <* spaces) <|> (try lineCommentParser <* spaces) 
              <|> (try commentParser <* spaces) <|> (try dispParser <* spaces) <|> (try dispAParser <* spaces) <|> (try showVParser <* spaces)
              <|> (try pushParser <* spaces) <|> (try popParser <* spaces) <|> (try ifParser <* spaces)

retInsParser :: Parser Operation
retInsParser = try doShParser

defMlMacroParser :: Parser Operation
defMlMacroParser = DefMlM <$> (string "def" >> many1 space >> many1 letter) <*> (spaces >> char '=' >> spaces >> char '{' >> spaces >> many1 insParser <* spaces <* char '}')

defMlMPacroParser :: Parser Operation
defMlMPacroParser = DefMlMP <$> (string "def" >> many1 space >> many1 letter) <*> (spaces >> char '(' >> spaces >> (many1 letter `sepBy` (char ',')) <* spaces <* char ')') <*> (spaces >> char '=' >> spaces >> char '{' >> spaces >> many1 insParser <* spaces <* char '}')

ifParser :: Parser Operation
ifParser = If <$> (string "if" >> spaces >> (try onlyValParser <|>try retParser <|> retrParser)) <*> (spaces >> char '{' >> spaces >> many1 insParser <* spaces <* char '}')

insToIO :: Operation -> MacroTable -> MLMacroTable -> MLMPacroTable -> IO ()
insToIO (DoSh command) _ _ _= void $ system command
insToIO (DispA val) _ _ _= print $ val
insToIO (Disp val) mT _ _= print (valToBin val mT)
insToIO (ShowV x [(Prompt str)]) mt mlm mp=  do
                                        putStrLn str
                                        inp <- getLine 
                                        insToIO (ShowV x [(Str inp)]) mt mlm mp
insToIO (ShowV x [(Filter y [(Prompt str)])]) mt mlm mp=  do
                                        putStrLn str
                                        inp <- getLine 
                                        insToIO (ShowV x [Filter y [(Str inp)]]) mt mlm mp
insToIO (ShowV x [(FileCon y)]) mt mlm mp = do
                                          fCon <- B.readFile y
                                          insToIO (ShowV x [(Str (word8ListToString $ B.unpack $ fCon))]) mt mlm mp
insToIO (ShowV x [(Rev (FileCon y))]) mt mlm mp = do
                                          fCon <- B.readFile y
                                          insToIO (ShowV x [(Str (reverse $ word8ListToString $ B.unpack $ fCon))]) mt mlm mp
insToIO (ShowV x [(Sort (FileCon y))]) mt mlm mp = do
                                          fCon <- B.readFile y
                                          insToIO (ShowV x [(Str (sort $ word8ListToString $ B.unpack $ fCon))]) mt mlm mp
insToIO (ShowV x [(Count (FileCon y))]) mt mlm mp = do
                                          fCon <- B.readFile y
                                          insToIO (ShowV x [(Int (show $ length $ B.unpack $ fCon))]) mt mlm mp
insToIO (ShowV x [(Filter z [(FileCon y)])]) mt mlm mp = do
                                          fCon <- B.readFile y
                                          insToIO (ShowV x [(Str (word8ListToString $ valToBin (Filter z [(Str (word8ListToString $ B.unpack $ fCon))]) mt))]) mt mlm mp
insToIO (ShowV x [(Filter xyz [((Filter z [(FileCon y)]))])]) mt mlm mp = do
                                          fCon <- B.readFile y
                                          insToIO (ShowV x [(Str (word8ListToString $ valToBin (Filter xyz [(Filter z [(Str (word8ListToString $ B.unpack $ fCon))])]) mt))]) mt mlm mp
insToIO (ShowV "str" val) mt _ _=( mapM_ (\v -> putStr $ word8ListToString $ (valToBin v mt)) val ) >>= \_ -> putStrLn $ ""
--insToIO (ShowV "chars" val) mT _ _=mapM_ (\v -> mapM_ (\i -> putStr $ "['" ++ (word8ListToString $ [i]) ++ "']") (valToBin v mT) >> putStrLn "") val
insToIO (ShowV "chars" val) mT _ _=putStrLn $ intercalate ", " (concat $ map (\v -> map (\x ->word8ListToString $ [x]) (valToBin v mT)) val)
insToIO (ShowV "int" val) mT _ _=(mapM_ (\v -> putStr $ show $ word8ListToInt $ (valToBin v mT)) val) >>= \_ -> putStrLn $ ""
insToIO (ShowV "intArr" val) mT _ _=putStrLn $ intercalate ", " (map (\v ->show $ valToBin v mT) val)
insToIO (ShowV "hexArr" val) mT _ _=putStrLn $ intercalate ", " (concat $ map (\v -> map (\x ->"0x"++(intToHex $ word8ListToInt $ [x])) (valToBin v mT)) val)
insToIO (ShowV "hex" val) mT _ _=(mapM_ (\v -> putStr $ intToHex $ word8ListToInt $ (valToBin v mT)) val) >>= \_ -> putStrLn $ ""
insToIO (ShowV "oct" val) mT _ _=(mapM_ (\v -> putStrLn $ intToOct $ word8ListToInt $ (valToBin v mT)) val) >>= \_ -> putStrLn $ ""
insToIO (ShowV "octArr" val) mT _ _=putStrLn $ intercalate ", " (concat $ map (\v -> map (\x ->"0o"++(intToOct $ word8ListToInt $ [x])) (valToBin v mT)) val)
insToIO (ShowV "bin" val) mT _ _=(mapM_ (\v -> putStr $ intToBin $ word8ListToInt $ (valToBin v mT)) val) >>= \_ -> putStrLn $ ""
insToIO (ShowV "binArr" val) mT _ _=putStrLn $ intercalate ", " (concat $ map (\v -> map (\x ->"0b"++(intToBin $ word8ListToInt $ [x])) (valToBin v mT)) val)
insToIO (ShowV "bool" val) mT _ _|(word8ListToInt $ (valToBin (val!!0) mT))==1=putStrLn $ "True"
                                 |(word8ListToInt $ (valToBin (val!!0) mT))==0=putStrLn $ "False"
                                 |otherwise=putStrLn $ "Not a Boolean"
insToIO (UseMLM name) mT mlmtable mlmp= case  (lookup name mlmtable ) of
                        Just value -> mapM_ (\operation -> insToIO operation mT mlmtable mlmp) value
                        Nothing -> error $ "This macro doesn't exist!"

insToIO (UseMLMP name vals) mT mlmtable mlmp= case  (lookup name mlmp ) of
                        Just value -> mapM_ (\sv -> insWparamsToIO sv  mT mlmtable (zip (fst $ value) vals) ) (snd $ value)
                        Nothing -> error $ "This macro doesn't exist!"

insToIO (If (Ret (DoSh command)) operations) mT mlmtable mlmp
                = system command >>= \exitCode -> case exitCode of
                                                    ExitSuccess -> mapM_ (\op -> insToIO op mT mlmtable mlmp) operations
                                                    ExitFailure n -> pure () --putStrLn $ "Command '"++command++"' failed with exit code: "++(show $ n)
insToIO (If (Retr (DoSh command)) operations) mT mlmtable mlmp
                = system command >>= \exitCode -> case exitCode of
                                                    ExitSuccess -> pure ()
                                                    ExitFailure n -> mapM_ (\op -> insToIO op mT mlmtable mlmp) operations --putStrLn $ "Command '"++command++"' failed with exit code: "++(show $ n)
insToIO (If statement operations) mT mlmtable mlmp |(word8ListToInt $ (valToBin statement mT))==1=mapM_ (\op -> insToIO op mT mlmtable mlmp) operations
                                                   |(word8ListToInt $ (valToBin statement mT))==0=pure ()
                                                   |otherwise=error $ "Wrong value in if statement"
insToIO(Foreach param vals operations) mT mlm mlmp = mapM_ (\val -> mapM_ (\op -> insWparamsToIO op mT mlm [(param,val)]) operations) vals
insToIO _ _ _ _= pure ()

insWparamsToIO :: Operation -> MacroTable -> MLMacroTable -> [(String,Value)] -> IO ()
insWparamsToIO (ShowV a b) mt mlm mp= (insToIO (ShowV a [(Str (word8ListToString $ concat $map (\x -> valOrParam x mp mt) b))] ) mt mlm [])
insWparamsToIO (If a operations) mt mlm mp= (insToIO (If (Str (word8ListToString $ concat $map (\x -> valOrParam x mp mt) [a]) ) operations) mt mlm [])
insWparamsToIO a b c d = insToIO a b c []

macroTable :: Operation -> MacroTable
macroTable (DefM name val)= [(name,val)]
macroTable _ =[]  

multiLineMacroTable :: Operation -> MLMacroTable
multiLineMacroTable (DefMlM name mlm) = [(name,mlm)]
multiLineMacroTable _ = []

multiLineMacroParameterTable :: Operation -> MLMPacroTable
multiLineMacroParameterTable (DefMlMP name params mlm) = [(name,(params,mlm))]
multiLineMacroParameterTable _ = []

shadowNames :: Operation -> [String]
shadowNames (Shadow name) = [name]
shadowNames _ = []

shadowTable :: Operation -> [String] -> ShadowTable
shadowTable (DefAs shadowName macroName ops) shadowsList = if (elem shadowName shadowsList) then [(shadowName, macroName, ops)]
                                                           else error $ "This shadow does not exist"
shadowTable _ _= []

replaceShadows :: Operation -> ShadowTable -> [Operation]
replaceShadows (UseAs s1 s2) list = 
  let matchingTuple = filter (\(a,b,_) -> a == s1 && b == s2) list
  in  case matchingTuple of
        (a, b, op):_ -> op
        _ -> error "This shadow does not exist"
replaceShadows (DefAs _ _ _) _ = []
replaceShadows (Shadow _) _ = []
replaceShadows a _ = [a]

includeFiles :: FilePath -> FilePath -> IO String
includeFiles fileName folder = do
  contents <- readFile (folder ++ "/" ++ fileName)
  let newIncludes = getIncludes contents
  if null newIncludes
    then return contents
    else do
      includedContents <- mapM (\ni -> includeFiles ni folder) newIncludes
      return $ concat includedContents ++ contents

getIncludes :: String -> [String]
getIncludes = map (drop 8) . filter ("include " `isPrefixOf`) . lines

getDir :: FilePath -> FilePath
getDir path = takeDirectory $ path

getFileName :: FilePath -> FilePath
getFileName path = takeFileName $ path

getFileNameWithoutExt :: FilePath -> FilePath
getFileNameWithoutExt path = takeBaseName $ path
