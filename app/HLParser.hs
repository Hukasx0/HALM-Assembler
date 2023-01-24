{-# LANGUAGE CPP #-}
module HLParser where

import Text.Parsec
import Text.Parsec.String
import System.Exit
import System.Process
import System.FilePath
import Control.Monad(void)
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
showVParser = ShowV <$> (string "show" >> many1 space >> (try (string "str") <|> try (string "chars") <|> try (string "int")<|> try (string "hexArr")
                    <|> try (string "hex") <|>try (string "octArr") <|> try (string "oct") <|> try (string "binArr") <|> try (string "bin") <|> try (string "bool") 
                    )) <*> (many1 space >> anyValParser)

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
useAsParser = UseAs <$> (string "Use" >> many1 space >> many1 letter) <*> (spaces >> string "->" >> spaces >> many1 letter)

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

ifParser :: Parser Operation
ifParser = If <$> (string "if" >> spaces >> (try onlyValParser <|>try retParser <|> retrParser)) <*> (spaces >> char '{' >> spaces >> many1 insParser <* spaces <* char '}')

insToIO :: Operation -> MacroTable -> MLMacroTable -> IO ()
insToIO (DoSh command) _ _= void $ system command
insToIO (DispA val) _ _= print $ val
insToIO (Disp val) mT _= print (valToBin val mT)
insToIO (ShowV "str" val) mt _ = putStrLn $ word8ListToString $ (valToBin val mt)
insToIO (ShowV "chars" val) mT _ = mapM_ (\i -> putStr $ "['" ++ (word8ListToString $ [i]) ++ "']") (valToBin val mT) >> putStrLn ""
insToIO (ShowV "int" val) mT _ = putStrLn $ show $ word8ListToInt $ (valToBin val mT)
insToIO (ShowV "hexArr" val) mT _ = mapM_ (\i -> putStr $ "[0x" ++ (intToHex $ word8ListToInt $ [i]) ++ "]") (reverse $ valToBin val mT) >> putStrLn ""
insToIO (ShowV "hex" val) mT _ = putStrLn $ intToHex $ word8ListToInt $ (valToBin val mT)
insToIO (ShowV "oct" val) mT _ = putStrLn $ intToOct $ word8ListToInt $ (valToBin val mT)
insToIO (ShowV "octArr" val) mT _ = mapM_ (\i -> putStr $ "[0o" ++ (intToOct $ word8ListToInt $ [i]) ++ "]") (reverse $ valToBin val mT) >> putStrLn ""
insToIO (ShowV "bin" val) mT _ = putStrLn $ intToBin $ word8ListToInt $ (valToBin val mT)
insToIO (ShowV "binArr" val) mT _ = mapM_ (\i -> putStr $ "[0b" ++ (intToBin $ word8ListToInt $ [i]) ++ "]") (reverse $ valToBin val mT) >> putStrLn ""
insToIO (ShowV "bool" val) mT _ |(word8ListToInt $ (valToBin val mT))==1=putStrLn $ "True"
                                 |(word8ListToInt $ (valToBin val mT))==0=putStrLn $ "False"
                                 |otherwise=putStrLn $ "Not a Boolean"
insToIO (UseMLM name) mT mlmtable= case  (lookup name mlmtable ) of
                        Just value -> mapM_ (\operation -> insToIO operation mT mlmtable) value
                        Nothing -> error $ "This macro doesn't exist!"
insToIO (If (Ret (DoSh command)) operations) mT mlmtable 
                = system command >>= \exitCode -> case exitCode of
                                                    ExitSuccess -> mapM_ (\op -> insToIO op mT mlmtable) operations
                                                    ExitFailure n -> pure () --putStrLn $ "Command '"++command++"' failed with exit code: "++(show $ n)
insToIO (If (Retr (DoSh command)) operations) mT mlmtable 
                = system command >>= \exitCode -> case exitCode of
                                                    ExitSuccess -> pure ()
                                                    ExitFailure n -> mapM_ (\op -> insToIO op mT mlmtable) operations --putStrLn $ "Command '"++command++"' failed with exit code: "++(show $ n)
insToIO (If statement operations) mT mlmtable |(word8ListToInt $ (valToBin statement mT))==1=mapM_ (\op -> insToIO op mT mlmtable) operations
                                              |(word8ListToInt $ (valToBin statement mT))==0=pure ()
                                              |otherwise=error $ "Wrong value in if statement"
insToIO _ _ _= pure ()

macroTable :: Operation -> MacroTable
macroTable (DefM name val)= [(name,val)]
macroTable _ =[]  

multiLineMacroTable :: Operation -> MLMacroTable
multiLineMacroTable (DefMlM name mlm) = [(name,mlm)]
multiLineMacroTable _ = []

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
