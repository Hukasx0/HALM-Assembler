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

codeToIns :: [Operation] -> MacroTable -> MLMacroTable -> LabelTable -> [[Word8]]
codeToIns code macroT mlm labelTbl= map (\c -> insToBin c macroT mlm labelTbl) code

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
              <|> try subParser <|> try negParser <|> try xorParser <|>try defLabelParser
              <|> includeParser

replaceStrings :: String -> String -> String
replaceStrings input rep = T.unpack $ T.intercalate (T.pack rep) (T.splitOn (T.pack "$filePath") (T.pack $ input))

isWinRep :: String -> String
isWinRep input = T.unpack $ T.intercalate (T.pack $ show $ isWindows) (T.splitOn (T.pack "$isWindows") (T.pack $ input))

isUnixRep :: String -> String
isUnixRep input= T.unpack $ T.intercalate (T.pack $ show $ isUnix) (T.splitOn (T.pack "$isUnix") (T.pack $ input))

main :: IO ()
main = do
          fileName <- head <$> getArgs
          fContent <- (includeFiles (getFileName $ fileName) (getDir $ fileName))
          putStrLn $ ("input:\n") 
          let content = libList ++ (isUnixRep $ isWinRep $ (replaceStrings fContent fileName))
          putStrLn $ content
          let parsed = parse (spaces >> many (finalParser <* many1 space) <* eof) fileName content
          putStrLn $ "output:"
          case parsed of
            Left err -> print err
            Right corr -> do
                          let mTable = (concat $ map macroTable corr)
                          let mlmTable = (concat $ map multiLineMacroTable corr)
                          let byteTuple = (zip (concat $ (map (\x -> byteFilter $ x) corr)) (reverse $ sumList $ concat $ (map (\op -> getCurrBytes op mTable mlmTable) corr)))
                          let fillBDone = map (fillBFilter) byteTuple
                          let labelTable = concat $ map (getLabelTable) byteTuple
                          let final = replaceValues corr (concat $ (map clearData fillBDone))        
                          putStrLn ("Writing binary to "++fileName++".bin")
                          B.writeFile (fileName++".bin") (B.pack $ concat $ (codeToIns final mTable mlmTable labelTable))
                          print (concat $ (codeToIns final mTable mlmTable labelTable))
                          codeToIO corr mTable mlmTable
