module Main where

import Text.Parsec
import Text.Parsec.String
import Data.Word
import System.Environment
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B

import Values
import InsParser
import HLParser

codeToIns :: [Operation] -> MacroTable -> MLMacroTable -> [[Word8]]
codeToIns code macroT mlm= map (\c -> insToBin c macroT mlm) code

codeToIO :: [Operation] -> MacroTable -> MLMacroTable -> IO ()
codeToIO code macroT mlm= mapM_ (\c -> insToIO c macroT mlm) code

finalParser :: Parser Operation
finalParser = try movParser <|> try interruptParser <|> try incParser <|> try decParser 
              <|> try cmpParser <|> try jmpParser <|> try jeParser <|> try jneParser 
              <|> try jgParser <|> try jgeParser <|> try jlParser <|> try jleParser
              <|> try addByParser <|> try doShParser <|> try lineCommentParser 
              <|> try commentParser <|>try dispParser <|>try dispAParser
              <|> try showVParser <|> try defMacroParser <|> try defMlMacroParser 
              <|> try useMLMParser <|> try fillBytesParser <|> try addParser 
              <|> try subParser <|> try negParser <|> xorParser

replaceStrings :: String -> String -> String
replaceStrings input rep = T.unpack $ T.intercalate (T.pack rep) (T.splitOn (T.pack "$filePath") (T.pack $ input))

main :: IO ()
main = do
          fileName <- head <$> getArgs
          fContent <- readFile $ fileName
          putStrLn $ ("input:\n") 
          let content = libList ++ (replaceStrings fContent fileName)
          putStrLn $ content
          let parsed = parse (spaces >> many (finalParser <* many1 space) <* eof) fileName content
          putStrLn $ "output:"
          case parsed of
            Left err -> print err
            Right corr -> do
                          let mTable = (concat $ map macroTable corr)
                          let mlmTable = (concat $ map multiLineMacroTable corr)
                          let fillBDone = map (\c -> fillBFilter c (reverse $ sumList $ (map (\op -> getCurrBytes op mTable mlmTable) corr))) corr
                          putStrLn ("Writing binary to "++fileName++".bin")
                          B.writeFile (fileName++".bin") (B.pack $ concat $ (codeToIns fillBDone mTable mlmTable))
                          print (concat $ (codeToIns fillBDone mTable mlmTable))
                          codeToIO corr mTable mlmTable
