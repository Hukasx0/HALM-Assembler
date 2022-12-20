module Main where

import Text.Parsec
import Data.Word
import qualified Data.ByteString.Lazy as B

import Values
import InsParser

codeToIns :: [Operation] -> [[Word8]]
codeToIns code = map insToBin code

main :: IO ()
main = do putStrLn $ ("input:\n"++virtualFile) 
          let parsed = parse (spaces >> many (finalParser <* many1 space) <* eof) "virtual" virtualFile
          putStrLn $ "output:"
          case parsed of
            Left err -> print err
            Right corr -> do
                          putStrLn "Writing binary to out.bin"
                          B.writeFile "out.bin" (B.pack $ concat $ codeToIns $ corr)
                          print (concat $ codeToIns $ corr)
 