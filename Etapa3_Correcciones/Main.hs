module Main(main) where

import Parser
import Lexer
import System.Environment   
import System.IO

main :: IO ()
main = do
	[f] <- getArgs
	s <- readFile f
	print $ parser (alexScanTokens s)