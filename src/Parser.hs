import Types
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

languageDef = 
  emptyDef { Token.commentStart     = "/*"
           , Token.commendEnd       = "*/"
		   , Token.commentLine      = "//"
		   , Token.identStart       = letter
		   , Token.identLetter      = alphaNum
		   , Token.reservedNames    = [ "fn"
		                              , "while"
								      , "if"
									  , "else"
								      , "Pure"
								      , "Impure"
								      , "IO"
								      , "WS"
								      , "RS"
								      , "true"
								      , "false"
								      , "Bool"
								      , "Int"
								      , "String"
								      , "Char"
									  , "let"
									  , "mut"
									  , "return"
									  ]
			, Token.reservedOpNames = ["+", "-", "*", "/", "%", "!",
			                           "=", "==", "!=", ">", "<", "<=",
									   ">=", "&&", "||", "nor", "nand",
									   "xor", "xnor", "[", "]", "{", "}"]
			}

lexer = Token.makeTokenParser languageDef
id = Token.identidier lexer
reserved = Token.reserved lexer
reservedOp = Token.reservedOp lexer
parens = Token.parens lexer
integer = Token.integer
semi = Token.semi
whiteSpace = Token.whiteSpace lexer

psiParser :: Parser Stmt
psiParser = whiteSpace >> statement

statement :: Parser Stmt
statement = parens statement
          <|> sequenceOfStatements

sequenceOfStmt = 
  do list <- (sepBy1 statement' semi)
     return (if length list == 1 then head list else Seq list)

statement' :: Parser Stmt
statement' =   ifStmt
           <|> whileStmt
		   <|> emptyStmt
		   <|> assignStmt
		   <|> declareStmt
		   <|> fnCallStmt
		   <|> fnDeclStmt
		   <|> fnImplStmt

ifStmt :: Parser Stmt
ifStmt = 
  do reserved "if"
     predicate <- BoolExpression
	 reservedOp "{"
	 trueStmt <- statement
	 reservedOp "}"
	 reserved "else"
	 reservedOp "{"
	 falseStmt <- statement
     reservedOp "}"
	 return $ If predicate trueStmt falseStmt

whileStmt :: Parser Stmt
whileStmt =
  do reserved "while"
     predicate <- BoolExpression
	 reservedOp "{"
	 body <- statement
	 reservedOp "}"
	 return $ While predicate body

emptyStmt :: Parser Stmt
emptyStmt =
  do reservedOp "{"
     reservedOp "}"
	 return Empty

assignStmt :: Parser Stmt
assignStmt =
  do s <- id
	 reservedOp "="
	 expr <- anyExpr
	 semi
	 return $ Assign s expr


-- Allow for mutable?
declareStmt :: Parser Stmt
declareStmt =
  do reserved "let"
	 t <- anyType
	 var <- id
	 reservedOp "="
     expr <- anyExpr
	 semi
	 return $ Declare True t var expr

fnCallStmt :: Parser Stmt

fnDeclStmt :: Parser Stmt

fnImplStmt :: Parser Stmt
