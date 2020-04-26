module Parser where

import Control.Monad
import Prelude hiding (id, EQ, LT, GT)
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
        --, "["
        --, "]"
        --, "{"
        --, "}"
        --, "::"
        --, "->"
        --, "\""
        --, ","
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
  char '{'
  trueStmt <- statement
  char '}'
  reserved "else"
  char '{'
  falseStmt <- statement
  char '}'
  return $ If predicate trueStmt falseStmt

whileStmt :: Parser Stmt
whileStmt = do
  reserved "while"
  predicate <- boolExpression
  char '{'
  body <- sequenceOfStmt
  char '}'
  return $ While predicate body

emptyStmt :: Parser Stmt
emptyStmt = do
  char '{'
  char '}'
  return Empty

assignStmt :: Parser Stmt
assignStmt = do
  s <- id
  reservedOp "="
  Assign s <$> parseExpr

declareStmt :: Parser Stmt
declareStmt = do
  reserved "let"
  mutFlag <- optionMaybe (reserved "mut")
  let isMut = maybe False (const True) mutFlag
  t <- parseType
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
                string "::"
                e <- parseEffect
                char '['
                types <- parseTypes
                string "->"
                t <- parseType
                char ']'
                return $ FnDecl name e types t

fnImplStmt :: Parser Stmt
fnImplStmt = do reserved "fn"
                name <- id
                args <- parseArgNames
                char '{'
                stmts <- sequenceOfStmt
                char '}'
                return $ FnImpl name args stmts

parseArgs = sepBy1 parseExpr $ char ','
parseArgNames = sepBy1 id $ char ','
parseEffect = do x <- id 
                 case x of 
                   "Pure" -> return Pure
                   "Impure" -> return Impure
                   "IO" -> return IO
                   "RS" -> return RS
                   "WS" -> return WS
                   _    -> fail $ "Invalid effect given: " ++ x

parseTypes = sepBy1 parseType $ char ',' 
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
boolExpression = buildExpressionParser boolOperators boolTerm

stringExpression :: Parser Expr
stringExpression = do char '"'
                      s <- many (noneOf "\"")
                      char '"'
                      return $ StringLit s

numExpression :: Parser Expr
numExpression = buildExpressionParser numOperators numTerm

numOperators = [ [Prefix (reservedOp "-"  >> return (Neg       )) ]
               , [Infix  (reservedOp "*"  >> return (NumBin Mul)) AssocLeft,
                  Infix  (reservedOp "/"  >> return (NumBin Div)) AssocLeft,
                  Infix  (reservedOp "%"  >> return (NumBin Mod)) AssocLeft]
               , [Infix  (reservedOp "+"  >> return (NumBin Add)) AssocLeft,
                  Infix  (reservedOp "-"  >> return (NumBin Sub)) AssocLeft]
               ]

boolOperators = [ [Prefix (reservedOp "!"    >> return (Not         )) ]
                , [Infix  (reservedOp "&&"   >> return (BoolBin And )) AssocLeft,
                   Infix  (reservedOp "||"   >> return (BoolBin Or  )) AssocLeft,
                   Infix  (reservedOp "nor"  >> return (BoolBin Nor )) AssocLeft,
                   Infix  (reservedOp "nand" >> return (BoolBin Nand)) AssocLeft,
                   Infix  (reservedOp "xor"  >> return (BoolBin Xor )) AssocLeft,
                   Infix  (reservedOp "xnor" >> return (BoolBin Xnor)) AssocLeft]
                ]

numTerm =  parens numExpression
       <|> liftM Var id
       <|> liftM IntLit integer

boolTerm =  parens boolExpression
        <|> (reserved "true"  >> return (BoolLit True ))
        <|> (reserved "false" >> return (BoolLit False))
        <|> relExpression

relExpression = do a  <- numExpression
                   op <- relationalOp
                   b  <- numExpression
                   return $ BoolRel op a b

relationalOp =   (reservedOp ">"  >> return LT)
             <|> (reservedOp ">"  >> return GT)
             <|> (reservedOp "==" >> return EQ)
             <|> (reservedOp "!=" >> return NE)
             <|> (reservedOp ">=" >> return GE)
             <|> (reservedOp "<=" >> return LE)
