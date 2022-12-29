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

defMacroParser :: Parser Operation
defMacroParser = DefM <$> (string "def" >> many1 space >> many1 letter) <*> (spaces >> char '=' >> spaces >> anyValParser)

insToIO :: Operation -> MacroTable -> IO ()
insToIO (DoSh command) _ = void $ system command
insToIO (DispA val) _= print $ val
insToIO (Disp val) mT= print (valToBin val mT)
insToIO _ _= pure ()

macroTable :: Operation -> [(String,Value)]
macroTable (DefM name val)= [(name,val)]
macroTable _ =[]  

