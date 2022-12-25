module Main where

import Text.Parsec
import Text.Parsec.String
import Data.Word
import System.Environment
import qualified Data.ByteString.Lazy as B

import Values
import InsParser
import HLParser

codeToIns :: [Operation] -> [[Word8]]
codeToIns code = map insToBin code

codeToIO :: [Operation] -> IO ()
codeToIO code = mapM_ insToIO code

finalParser :: Parser Operation
finalParser = try movParser <|> try interruptParser <|> try incParser <|> try decParser 
              <|> try cmpParser <|> try jmpParser <|> try addByParser <|> try doShParser 
              <|> try lineCommentParser <|> commentParser

main :: IO ()
main = do
          fileName <- head <$> getArgs
          fContent <- readFile $ fileName
          putStrLn $ ("input:\n"++fContent) 
          let parsed = parse (spaces >> many (finalParser <* many1 space) <* eof) fileName fContent
          putStrLn $ "output:"
          case parsed of
            Left err -> print err
            Right corr -> do
                          putStrLn ("Writing binary to "++fileName++".bin")
                          B.writeFile (fileName++".bin") (B.pack $ concat $ codeToIns $ corr)
                          print (concat $ codeToIns $ corr)
                          codeToIO $ corr
 