module Main(main) where

import Parser
import Lexer
import SymbolChecking
import SymbolTable
import System.Environment   
import System.IO

main :: IO ()
main = do
	[f] <- getArgs
	s <- readFile f
	let info = parser (alexScanTokens s)
	let Resultado (st, errs) = process info (Resultado ([], []))
	if not $ null errs
		then mapM_ putStrLn errs
		else return ()
	