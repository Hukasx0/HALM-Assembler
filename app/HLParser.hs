module HLParser where

import Text.Parsec
import Text.Parsec.String
import System.Process
import Control.Monad(void)
import Values
import ValParser

doShParser :: Parser Operation
doShParser = DoSh <$> (string "doSh" >> spaces >> char '=' >> spaces >> pureStringParser)

insToIO :: Operation -> IO ()
insToIO (DoSh command) = void $ system command
insToIO _ = pure ()
