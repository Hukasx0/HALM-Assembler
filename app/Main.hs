module Main where

import Text.Parsec
import Data.Word
import System.Environment
import qualified Data.ByteString.Lazy as B

import Values
import InsParser

codeToIns :: [Operation] -> [[Word8]]
codeToIns code = map insToBin code

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
 