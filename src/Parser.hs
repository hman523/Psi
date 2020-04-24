module Parser where

import Control.Monad
import Prelude hiding (id)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import Types

languageDef =
  emptyDef
    { Token.commentStart = "/*"
    , Token.commentEnd = "*/"
    , Token.commentLine = "//"
    , Token.identStart = letter
    , Token.identLetter = alphaNum
    , Token.reservedNames =
        [ "fn"
        , "while"
        , "if"
        , "else"
      --  , "Pure"
      --  , "Impure"
      --  , "IO"
      --  , "WS"
      --  , "RS"
        , "true"
        , "false"
      --  , "Bool"
      --  , "Int"
      --  , "String"
      --  , "Char"
        , "let"
        , "mut"
        , "return"
        ]
    , Token.reservedOpNames =
        [ "+"
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
        , "["
        , "]"
        , "{"
        , "}"
        , "::"
        , "->"
        , "\""
        , ","
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

psiParser :: Parser Stmt
psiParser = whiteSpace >> statement

statement :: Parser Stmt
statement = parens statement <|> sequenceOfStmt

sequenceOfStmt = do
  list <- sepEndBy1 statement' semi
  return $ Body list

statement' :: Parser Stmt
statement' =
  try ifStmt <|> try whileStmt <|> try assignStmt <|> try declareStmt <|>
  try (ExprLit <$> fnCallStmt) <|>
  try fnDeclStmt <|>
  fnImplStmt

ifStmt :: Parser Stmt
ifStmt = do
  reserved "if"
  predicate <- boolExpression
  reservedOp "{"
  trueStmt <- statement
  reservedOp "}"
  reserved "else"
  reservedOp "{"
  falseStmt <- statement
  reservedOp "}"
  return $ If predicate trueStmt falseStmt

whileStmt :: Parser Stmt
whileStmt = do
  reserved "while"
  predicate <- boolExpression
  reservedOp "{"
  body <- sequenceOfStmt
  reservedOp "}"
  return $ While predicate body

emptyStmt :: Parser Stmt
emptyStmt = do
  reservedOp "{"
  reservedOp "}"
  return Empty

assignStmt :: Parser Stmt
assignStmt = do
  s <- id
  reservedOp "="
  expr <- parseExpr
  return $ Assign s expr

declareStmt :: Parser Stmt
declareStmt = do
  reserved "let"
  mutFlag <- optionMaybe (reserved "mut")
  let isMut = maybe False (const True) mutFlag
  t <- anyType
  var <- id
  reservedOp "="
  expr <- parseExpr
  return $ Declare isMut t var expr

fnCallStmt :: Parser Expr
fnCallStmt = do name <- id
                args <- parseArgs
                return $ FnCall name args

fnDeclStmt :: Parser Stmt
fnDeclStmt = do reserved "fn"
                name <- id
                reservedOp "::"
                e <- parseEffect
                reservedOp "["
                types <- parseTypes
                reservedOp "->"
                t <- parseType
                reservedOp "]"
                return $ FnDecl name e types t

fnImplStmt :: Parser Stmt
fnImplStmt = do reserved "fn"
                name <- id
                args <- parseArgNames
                reservedOp "{"
                stmts <- sequenceOfStmt
                reservedOp "}"
                return $ FnImpl name args stmts

parseArgs = sepBy1 parseExpr $ reserved ","
parseArgNames = sepBy1 id $ reserved ","
parseEffect = do x <- id 
                 case x of 
                   "Pure" -> return Pure
                   "Impure" -> return Impure
                   "IO" -> return IO
                   "RS" -> return RS
                   "WS" -> return WS
                   _    -> fail $ "Invalid effect given: " ++ x

parseTypes = sepBy1 parseType $ reserved "," 
parseType = do x <- id
               case x of
                 "Int" -> return IntT
                 "Bool" -> return BooleanT
                 "String" -> return StringT
                 "Char" -> return CharT
                 _ -> return $ TypeVar x
 
parseExpr :: Parser Expr 
parseExpr = try boolExpression <|> try numExpression <|> stringExpression

boolExpression :: Parser Expr
boolExpression = do x <- id
                    return $ Var x

stringExpression = do char '"'
                      s <- many (noneOf "\"")
                      char '"'
                      return $ StringLit s

numExpression :: Parser Expr
numExpression = undefined

anyType = id >> return BooleanT
-- numOperators = [ [Prefix (reserse
