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

codeToIns :: [Operation] -> MacroTable -> [[Word8]]
codeToIns code macroT = map (\c -> insToBin c macroT) code

codeToIO :: [Operation] -> MacroTable -> IO ()
codeToIO code macroT= mapM_ (\c -> insToIO c macroT) code

finalParser :: Parser Operation
finalParser = try movParser <|> try interruptParser <|> try incParser <|> try decParser 
              <|> try cmpParser <|> try jmpParser <|> try addByParser <|> try doShParser 
              <|> try lineCommentParser <|>try commentParser <|>try dispParser <|>try dispAParser
              <|> defMacroParser

replaceStrings :: String -> String -> String
replaceStrings input rep = T.unpack $ T.intercalate (T.pack rep) (T.splitOn (T.pack "$filePath") (T.pack $ input))

main :: IO ()
main = do
          fileName <- head <$> getArgs
          fContent <- readFile $ fileName
          putStrLn $ ("input:\n") 
          let content = replaceStrings fContent fileName
          putStrLn $ content
          let parsed = parse (spaces >> many (finalParser <* many1 space) <* eof) fileName content
          putStrLn $ "output:"
          case parsed of
            Left err -> print err
            Right corr -> do
                          let mTable = (concat $ map macroTable corr)
                          putStrLn ("Writing binary to "++fileName++".bin")
                          B.writeFile (fileName++".bin") (B.pack $ concat $ (codeToIns corr mTable))
                          print (concat $ (codeToIns corr mTable))
                          codeToIO corr mTable
 