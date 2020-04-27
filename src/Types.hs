module Types where

data Expr = Var String
          | IntLit Integer
          | Neg Expr
          | BoolLit Bool
          | Not Expr
          | BoolBin BoolBinOp Expr Expr
          | BoolRel BoolRelOp Expr Expr
          | NumBin ArithOp Expr Expr
          | StringLit String
          | FnCall String [Expr]
               deriving (Show)

-- Boolean operators for Bool -> Bool -> Bool
data BoolBinOp = And | Or | Nor | Nand | Xor | Xnor
                deriving (Show)

-- Numerical relation operators for Num -> Num -> Bool
data BoolRelOp = GT | LT | EQ | GE | LE | NE
                deriving (Show)

data ArithOp = Add | Sub | Mul | Div | Mod
              deriving (Show)

data PsiType = VoidT | BooleanT | IntT | StringT | CharT | TypeVar String
              deriving (Show)

data Effect = Pure | Impure | IO | RS | WS
             deriving (Show)

data Stmt = Empty
          | Body [Stmt]
          | Assign String Expr
          | Declare Bool PsiType String Expr
          | If Expr Stmt Stmt
          | While Expr Stmt
          | FnDecl String Effect [PsiType] PsiType
          | FnImpl String [String] Stmt
          | Return Expr
          | ExprLit Expr
           deriving (Show)

