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

codeToIns :: [Operation] -> MacroTable -> MLMacroTable -> LabelTable -> Int -> [[Word8]]
codeToIns code macroT mlm labelTbl org= map (\c -> insToBin c macroT mlm labelTbl org) code

codeToIO :: [Operation] -> MacroTable -> MLMacroTable -> MLMPacroTable -> IO ()
codeToIO code macroT mlm mlmp= mapM_ (\c -> insToIO c macroT mlm mlmp) code

finalParser :: Parser Operation
finalParser = try movParser <|> try interruptParser <|> try incParser <|> try decParser 
              <|> try cmpParser <|> try jmpParser <|> try jeParser <|> try jneParser 
              <|> try jgParser <|> try jgeParser <|> try jlParser <|> try jleParser
              <|> try addByParser <|> try doShParser <|> try lineCommentParser 
              <|> try commentParser <|>try dispParser <|>try dispAParser
              <|> try showVParser <|> try defMacroParser<|>try defMlMPacroParser<|> try defMlMacroParser 
              <|>try useMLMPParser<|> try useMLMParser <|> try fillBytesParser <|> try addParser 
              <|> try subParser <|> try negParser <|> try xorParser <|> try defLabelParser
              <|> try includeParser <|>try ifParser <|>try pushParser <|>try popParser
              <|> try shadowParser <|> try defAsParser <|>try useAsParser <|> setOriginParser

replaceFpath :: String -> String -> String
replaceFpath input rep = T.unpack $ T.intercalate (T.pack rep) (T.splitOn (T.pack "$filePath") (T.pack $ input))

replacefName :: String -> String -> String
replacefName input rep = T.unpack $ T.intercalate (T.pack rep) (T.splitOn (T.pack "$fileName") (T.pack $ input))

replacefNameNoExt :: String -> String -> String
replacefNameNoExt input rep = T.unpack $ T.intercalate (T.pack rep) (T.splitOn (T.pack "$name") (T.pack $ input))

isWinRep :: String -> String
isWinRep input = T.unpack $ T.intercalate (T.pack $ show $ isWindows) (T.splitOn (T.pack "$isWindows") (T.pack $ input))

isUnixRep :: String -> String
isUnixRep input= T.unpack $ T.intercalate (T.pack $ show $ isUnix) (T.splitOn (T.pack "$isUnix") (T.pack $ input))

main :: IO ()
main = do
          fileName <- head <$> getArgs
          fContent <- (includeFiles (getFileName $ fileName) (getDir $ fileName)) 
          let content = (isUnixRep $ isWinRep $ (replacefNameNoExt (replacefName (replaceFpath (libList ++ fContent) (getDir $ fileName)) (getFileName $ fileName))) (getFileNameWithoutExt $ fileName))
          --putStrLn $ content
          let parsed = parse (spaces >> many (finalParser <* many1 space) <* eof) fileName content
          case parsed of
            Left err -> print err
            Right corr -> do
                          let originVal = (sum $ (map (originFilter) corr))
                          let shadRep = concat $ map (\c -> replaceShadows c (concat $ map (\c -> shadowTable c (concat $ map shadowNames corr)) corr)) corr
                          let mTable = (concat $ map macroTable shadRep)
                          let mlmTable = (concat $ map multiLineMacroTable shadRep)
                          let mlmpTable = (concat $ map multiLineMacroParameterTable shadRep)
                          let byteTuple = (zip (concat $ (map (\x -> byteFilter $ x) shadRep)) (reverse $ sumList $ concat $ (map (\op -> getCurrBytes op mTable mlmTable originVal) shadRep)))
                          let fillBDone = map (fillBFilter) byteTuple
                          let labelTable = concat $ map (getLabelTable) byteTuple
                          let final = replaceValues shadRep (concat $ (map clearData fillBDone))        
                          putStrLn ("Writing binary to "++fileName++".bin")
                          putStrLn $ "IO output:"
                          B.writeFile (fileName++".bin") (B.pack $ concat $ (codeToIns final mTable mlmTable labelTable originVal))
                         -- print (concat $ (codeToIns final mTable mlmTable labelTable originVal))
                          codeToIO shadRep mTable mlmTable mlmpTable
