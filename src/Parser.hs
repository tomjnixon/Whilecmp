module Parser (mainParser, parseWhileFile) where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr

import While

whileDef = emptyDef { commentStart = "{"
                    , commentEnd = "}"
                    , identStart = letter
                    , identLetter = alphaNum
                    , opStart = oneOf "!&=<:;+-*\""
                    , opLetter = oneOf "!&=<:;+-*\""
                    , reservedOpNames = ["!", "&", "=", "<=", ":=",
                                         ";", "+", "-", "*", "(", ")"]
                    , reservedNames = ["true", "false", "skip",
                                       "if", "then", "else",
                                       "while", "do", "printf"]
                    }

lexer  = P.makeTokenParser whileDef

symbol     = P.symbol lexer
identifier = P.identifier lexer
reserved   = P.reserved lexer
reservedOp = P.reservedOp lexer
parens     = P.parens lexer
natural    = P.natural lexer
whiteSpace = P.whiteSpace lexer
semiSep1   = P.semiSep1 lexer
stringLiteral   = P.stringLiteral lexer

variableParser :: Parser Variable
variableParser = identifier >>= return . Variable

arithmeticParser = buildExpressionParser table term
	where
		table = [ [ Infix (reservedOp "+" >> return Add) AssocLeft ]
		        , [ Infix (reservedOp "-" >> return Sub) AssocLeft ]
		        , [ Infix (reservedOp "*" >> return Mul) AssocLeft ]
		        ]
		term = choice $ map try [ parens arithmeticParser
		                        , fmap AVariable variableParser
		                        , fmap Constant natural
		                        ]

booleanParser = buildExpressionParser table term
	where
		table = [ [ Prefix (reservedOp "!" >> return Not) ]
		        , [ Infix  (reservedOp "&" >> return And) AssocLeft ]
		        ]
		term = choice $ map try [ parens booleanParser
		                        ,  (reserved "true" >> return BTrue)
		                        ,  (reserved "false" >> return BFalse)
		                        ,  infixArith Equal "="
		                        ,  infixArith LTE "<="
		                        ]
		infixArith comb sym = do
			a <- arithmeticParser
			reservedOp sym
			b <- arithmeticParser
			return $ comb a b

singleStatementParser =
	choice $ map try [ parens statementParser
	                 , assignmentParser
	                 , skipParser
	                 , ifParser
	                 , whileParser
	                 , printfParser
	                 ]
		where
			assignmentParser = do
				x <- variableParser
				reservedOp ":="
				a <- arithmeticParser
				return $ Assignment x a
			skipParser = reserved "skip" >> return Skip
			ifParser = do
				reserved "if"
				b <- booleanParser
				reserved "then"
				s1 <- statementParser
				reserved "else"
				s2 <- statementParser
				return $ If b s1 s2
			whileParser = do
				reserved "while"
				b <- booleanParser
				reserved "do"
				s <- singleStatementParser
				return $ While b s
			printfParser = do
				reserved "printf"
				s <- stringLiteral
				args <- many arithmeticParser
				return $ Printf s args

statementParser = semiSep1 singleStatementParser >>= return . toTree
	where
		toTree [s] = s
		toTree (s:ss) = Sequence s $ toTree ss

mainParser = do
	whiteSpace
	stmt <- statementParser
	eof
	return stmt

parseWhileFile = parseFromFile mainParser
