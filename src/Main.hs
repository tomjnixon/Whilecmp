module Main where

import BackendInterpreter
import BackendX86
import Parser
import MiddleEnd
import System

main = do
	args <- getArgs
	parseArgs args

parseThen f_name next = do
	parse_result <- parseWhileFile f_name
	case parse_result of
		Left err ->
			print err
		Right program ->
			next program

doInterpret program = show_state $ run $ newState $ cs program
doCompile out program = do
	let am = cs program
	let code = compile am
	writeFile out code

help = do
	name <- getProgName
	mapM_ putStrLn
		[ "usage: " ++ name ++ " [ARGS]"
		, "  ARGS:"
		, "    -c in.while out.s"
		, "      Compile in.while to out.s."
		, "    -i in.while"
		, "      Interpret in.while."
		]

parseArgs ["-i", f_name] = parseThen f_name doInterpret
parseArgs ["-c", f_name, out_name] = parseThen f_name $ doCompile out_name 
parseArgs _ = help
