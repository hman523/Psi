module Types where

import qualified Data.Map                      as Map
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

-- Here we're giving stmt a better name for certain type signatures
type AST = Stmt

data Stmt = Empty
          | Body [Stmt]
          | Assign String Expr
          | Declare VarDecl
          | If Expr Stmt Stmt
          | While Expr Stmt
          | FuncDecl FnDecl
          | FuncImpl FnImpl
          | Return Expr
          | ExprLit Expr
           deriving (Show)

--             fn name  effect  parameter types  return type
type FnDecl = (String, Effect, [PsiType], PsiType)

--             fn name  parameter names  body
type FnImpl = (String, [String], Stmt)

--              mutabilty  type     name    value
type VarDecl = (Bool, PsiType, String, Expr)


type FnDeclTable = Map.Map String FnDecl
type FnImplTable = Map.Map String FnImpl
type ScopeTable = [Map.Map String VarDecl]
type ParentTable = [[Int]]
type VarDeclTable = (ScopeTable, ParentTable)

type Tables = (FnDeclTable, FnImplTable, VarDeclTable)

data BindError = UndeclaredFunction FnImpl
               | RedeclaredFunction FnDecl
               | RedefinedFunction FnImpl
               | RedeclaredVar VarDecl
                deriving (Show)


