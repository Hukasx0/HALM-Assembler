module HLParser where

import Text.Parsec
import Text.Parsec.String
import System.Process
import Control.Monad(void)
import Values
import ValParser

doShParser :: Parser Operation
doShParser = DoSh <$> (string "doSh" >> spaces >> char '=' >> spaces >> pureStringParser)

dispParser :: Parser Operation
dispParser = Disp <$> (string "disp" >> many1 space >> anyValParser)

dispAParser :: Parser Operation
dispAParser = DispA <$> (string "Disp" >> many1 space >> anyValParser)

lineCommentParser :: Parser Operation
lineCommentParser = Comment <$> (string "--" >> many (noneOf "\n"))

commentParser :: Parser Operation
commentParser = Comment <$> ( between (string "{-") (string "-}") (many (noneOf "-}")) )

insToIO :: Operation -> IO ()
insToIO (DoSh command) = void $ system command
insToIO (DispA val) = print $ val
insToIO (Disp val) = print (valToBin $ val)
insToIO _ = pure ()
