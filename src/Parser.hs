module Parser where

import           Control.Monad
import           Prelude                 hiding ( id
                                                , EQ
                                                , LT
                                                , GT
                                                )
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Expr
import           Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token
                                               as Token
import           Types
import           Data.Maybe                     ( isJust )

languageDef = emptyDef
  { Token.commentStart    = "/*"
  , Token.commentEnd      = "*/"
  , Token.commentLine     = "//"
  , Token.identStart      = letter
  , Token.identLetter     = alphaNum
  , Token.reservedNames   = [ "fn"
                            , "while"
                            , "if"
                            , "else"
                            , "true"
                            , "false"
                            , "let"
                            , "mut"
                            , "return"
                            ]
  , Token.reservedOpNames = [ "+"
                            , "-"
                            , "*"
                            , "/"
                            , "%"
                            , "!"
                            , "="
                            , "=="
                            , "!="
                            , ">"
                            , "<"
                            , "<="
                            , ">="
                            , "&&"
                            , "||"
                            , "nor"
                            , "nand"
                            , "xor"
                            , "xnor"
                            ]
  }

lexer = Token.makeTokenParser languageDef

id = Token.identifier lexer

reserved = Token.reserved lexer

reservedOp = Token.reservedOp lexer

parens = Token.parens lexer

integer = Token.integer lexer

semi = Token.semi lexer

whiteSpace = Token.whiteSpace lexer

symbol = Token.symbol lexer

parse :: String -> Either ParseError AST
parse = Text.ParserCombinators.Parsec.parse psiParser ""

psiParser :: Parser Stmt
psiParser = whiteSpace >> statement

statement :: Parser Stmt
statement = sequenceOfStmt

sequenceOfStmt = do
  list <- sepEndBy1 statement' semi
  return $ Body list

statement' :: Parser Stmt
statement' =
  try ifStmt
    <|> try returnStmt
    <|> try whileStmt
    <|> try assignStmt
    <|> try declareStmt
    <|> try (ExprLit <$> fnCallExpr)
    <|> try fnDeclStmt
    <|> fnImplStmt

returnStmt :: Parser Stmt
returnStmt =
  do
      reserved "return"
      Return <$> parseExpr
    <?> "return statement"

ifStmt :: Parser Stmt
ifStmt =
  do
      reserved "if"
      predicate <- boolExpression
      symbol "{"
      trueStmt <- sequenceOfStmt
      symbol "}"
      reserved "else"
      symbol "{"
      falseStmt <- sequenceOfStmt
      symbol "}"
      return $ If predicate trueStmt falseStmt
    <?> "if statement"

whileStmt :: Parser Stmt
whileStmt =
  do
      reserved "while"
      predicate <- boolExpression
      symbol "{"
      body <- sequenceOfStmt
      symbol "}"
      return $ While predicate body
    <?> "while statement"

emptyStmt :: Parser Stmt
emptyStmt =
  do
      symbol "{"
      symbol "}"
      return Empty
    <?> "empty body"

assignStmt :: Parser Stmt
assignStmt =
  do
      s <- id
      reservedOp "="
      Assign s <$> parseExpr
    <?> "assignment statement"

declareStmt :: Parser Stmt
declareStmt =
  do
      reserved "let"
      mutFlag <- optionMaybe (reserved "mut")
      let isMut = isJust mutFlag
      t   <- parseType
      var <- id
      reservedOp "="
      val <- parseExpr
      return $ Declare (isMut, t, var, val)
    <?> "variable declaration"

fnCallExpr :: Parser Expr
fnCallExpr = FnCall <$> id <*> parseArgs <?> "function call"

fnDeclStmt :: Parser Stmt
fnDeclStmt =
  do
      reserved "fn"
      name <- id
      symbol "::"
      e <- parseEffect
      symbol "["
      types <- parseTypes
      symbol "->"
      t <- parseType
      symbol "]"
      return $ FuncDecl (name, e, types, t)
    <?> "function declaration"

fnImplStmt :: Parser Stmt
fnImplStmt =
  do
      reserved "fn"
      name <- id
      args <- parens parseArgNames
      symbol "{"
      stmts <- sequenceOfStmt
      symbol "}"
      return $ FuncImpl (name, args, stmts)
    <?> "function implementation"

parseArgs = do
  symbol "("
  x <- sepBy parseExpr $ symbol ","
  symbol ")"
  return x

parseArgNames = sepBy id $ symbol ","
parseEffect =
  do
      x <- id
      case x of
        "Pure"   -> return Pure
        "Impure" -> return Impure
        "IO"     -> return IO
        "RS"     -> return RS
        "WS"     -> return WS
        _        -> fail $ "Invalid effect given: " ++ x
    <?> "effect"

parseTypes = sepBy1 parseType $ symbol ","
parseType =
  do
      x <- id
      case x of
        "Int"    -> return IntT
        "Bool"   -> return BooleanT
        "String" -> return StringT
        "Char"   -> return CharT
        "Void"   -> return VoidT
        _        -> return $ TypeVar x
    <?> "type"

parseExpr :: Parser Expr
parseExpr =
  (   try fnCallExpr
    <|> try boolExpression
    <|> try numExpression
    <|> stringExpression
    )
    <?> "expression"

boolExpression :: Parser Expr
boolExpression =
  buildExpressionParser boolOperators boolTerm <?> "boolean expression"

stringExpression :: Parser Expr
stringExpression =
  do
      char '"'
      s <- many (noneOf "\"")
      symbol "\""
      return $ StringLit s
    <?> "string literal"

numExpression :: Parser Expr
numExpression =
  buildExpressionParser numOperators numTerm <?> "numeric expression"

numOperators =
  [ [Prefix (reservedOp "-" >> return Neg)]
  , [ Infix (reservedOp "*" >> return (NumBin Mul)) AssocLeft
    , Infix (reservedOp "/" >> return (NumBin Div)) AssocLeft
    , Infix (reservedOp "%" >> return (NumBin Mod)) AssocLeft
    ]
  , [ Infix (reservedOp "+" >> return (NumBin Add)) AssocLeft
    , Infix (reservedOp "-" >> return (NumBin Sub)) AssocLeft
    ]
  ]

boolOperators =
  [ [Prefix (reservedOp "!" >> return Not)]
  , [ Infix (reservedOp "&&" >> return (BoolBin And))    AssocLeft
    , Infix (reservedOp "||" >> return (BoolBin Or))     AssocLeft
    , Infix (reservedOp "nor" >> return (BoolBin Nor))   AssocLeft
    , Infix (reservedOp "nand" >> return (BoolBin Nand)) AssocLeft
    , Infix (reservedOp "xor" >> return (BoolBin Xor))   AssocLeft
    , Infix (reservedOp "xnor" >> return (BoolBin Xnor)) AssocLeft
    ]
  ]

numTerm =
  parens numExpression
    <|> try fnCallExpr
    <|> fmap Var    id
    <|> fmap IntLit integer

boolTerm =
  parens boolExpression
    <|> try fnCallExpr
    <|> (reserved "true" >> return (BoolLit True))
    <|> (reserved "false" >> return (BoolLit False))
    <|> relExpression

relExpression = do
  a  <- numExpression
  op <- relationalOp
  BoolRel op a <$> numExpression

relationalOp =
  (reservedOp "<" >> return LT)
    <|> (reservedOp ">" >> return GT)
    <|> (reservedOp "==" >> return EQ)
    <|> (reservedOp "!=" >> return NE)
    <|> (reservedOp ">=" >> return GE)
    <|> (reservedOp "<=" >> return LE)
