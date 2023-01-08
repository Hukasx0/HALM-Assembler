module HLParser where

import Text.Parsec
import Text.Parsec.String
import System.Process
import System.FilePath
import Control.Monad(void)
import Data.List
import Values
import ValParser
import InsParser

doShParser :: Parser Operation
doShParser = DoSh <$> (string "doSh" >> spaces >> char '=' >> spaces >> pureStringParser)

dispParser :: Parser Operation
dispParser = Disp <$> (string "disp" >> many1 space >> anyValParser)

dispAParser :: Parser Operation
dispAParser = DispA <$> (string "Disp" >> many1 space >> anyValParser)

showVParser :: Parser Operation
showVParser = ShowV <$> (string "show" >> many1 space >> (try (string "str") <|> try (string "int") <|> try (string "hex") <|> try (string "oct") <|> try (string "bin"))) <*> (many1 space >> anyValParser)

lineCommentParser :: Parser Operation
lineCommentParser = Comment <$> (string "--" >> many (noneOf "\n"))

commentParser :: Parser Operation
commentParser = Comment <$> ( between (string "{-") (string "-}") (many (noneOf "-}")) )

defMacroParser :: Parser Operation
defMacroParser = DefM <$> (string "def" >> many1 space >> many1 letter) <*> (spaces >> char '=' >> spaces >> onlyValParser)

includeParser :: Parser Operation
includeParser = Incl <$> (string "include" >> many1 space >> many1 (noneOf " \n\r"))

insParser :: Parser Operation
insParser = (try movParser <* spaces) <|> (try interruptParser <* spaces) <|> (try incParser <* spaces) <|> (try decParser <* spaces) 
              <|> (try cmpParser <* spaces) <|> (try jmpParser <* spaces) <|> (try jeParser <* spaces) <|> (try jneParser <* spaces)
              <|> (try jgParser <* spaces) <|> (try jgeParser <* spaces) <|> (try jlParser <* spaces) <|> (try jleParser <* spaces)
              <|> (try addByParser <* spaces) <|> (try doShParser <* spaces) <|> (try lineCommentParser <* spaces) 
              <|> (try commentParser <* spaces) <|> (try dispParser <* spaces) <|> (try dispAParser <* spaces) <|> (try showVParser <* spaces)

defMlMacroParser :: Parser Operation
defMlMacroParser = DefMlM <$> (string "def" >> many1 space >> many1 letter) <*> (spaces >> char '=' >> spaces >> char '{' >> spaces >> many1 insParser <* spaces <* char '}')

insToIO :: Operation -> MacroTable -> MLMacroTable -> IO ()
insToIO (DoSh command) _ _= void $ system command
insToIO (DispA val) _ _= print $ val
insToIO (Disp val) mT _= print (valToBin val mT)
insToIO (ShowV "str" val) mt _ = putStrLn $ word8ListToString $ (valToBin val mt)
insToIO (ShowV "int" val) mT _ = putStrLn $ show $ word8ListToInt $ (valToBin val mT)
insToIO (ShowV "hex" val) mT _ = putStrLn $ intToHex $ word8ListToInt $ (valToBin val mT)
insToIO (ShowV "oct" val) mT _ = putStrLn $ intToOct $ word8ListToInt $ (valToBin val mT)
insToIO (ShowV "bin" val) mT _ = putStrLn $ intToBin $ word8ListToInt $ (valToBin val mT)
insToIO (UseMLM name) mT mlmtable= case  (lookup name mlmtable ) of
                        Just value -> mapM_ (\operation -> insToIO operation mT mlmtable) value
                        Nothing -> error $ "This macro doesn't exist!"
insToIO _ _ _= pure ()

macroTable :: Operation -> MacroTable
macroTable (DefM name val)= [(name,val)]
macroTable _ =[]  

multiLineMacroTable :: Operation -> MLMacroTable
multiLineMacroTable (DefMlM name mlm) = [(name,mlm)]
multiLineMacroTable _ = []

includeFiles :: FilePath -> FilePath -> IO String
includeFiles fileName folder = do
  contents <- readFile (folder ++ "/" ++ fileName)
  let newIncludes = getIncludes contents
  if null newIncludes
    then return contents
    else do
      includedContents <- mapM (\ni -> includeFiles ni folder) newIncludes
      return $ concat includedContents

getIncludes :: String -> [String]
getIncludes = map (drop 8) . filter ("include " `isPrefixOf`) . lines

getDir :: FilePath -> FilePath
getDir path = takeDirectory $ path

getFileName :: FilePath -> FilePath
getFileName path = takeFileName $ path
