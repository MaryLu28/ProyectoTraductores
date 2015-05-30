module Main(main) where

import Lexer
import System.Environment   
import System.Directory  
import System.IO

main :: IO ()
main = do
	[f] <- getArgs
	s <- readFile f
	print (alexScanTokens s)