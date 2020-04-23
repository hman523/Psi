module Types where

data BoolExpr = BoolVar String
              | BoolLit Bool
              | Not BoolExpr
			  | BoolBin BoolBinOp BoolExpr BoolExpr
			  | BoolRel BoolRelOp NumExpr NumExpr
			   deriving (Show)

-- Boolean operators for Bool -> Bool -> Bool
data BoolBinOp = And | Or | Nor | Nand | Xor | Xnor
                deriving (Show)

-- Numerical relation operators for Num -> Num -> Bool
data BoolRelOp = GT | LT | EQ | GE | LE | NE
                deriving (Show)

data NumExpr = NumVar String
             | IntLit Int
			 | Neg NumExpr
			 | NumBin ArithOp NumExpr NumExpr
			  deriving (Show)
			  
data ArithOp = Add | Sub | Mul | Div | Mod 
              deriving (Show)

data PsiType = BooleanT | IntT | StringT | CharT 
              deriving (Show)

data Effect = Pure | Impure | IO | RS | WS
             deriving (Show)

data StringExpr = StrVar String
                | StrLit String
				 deriving (Show)

data Expr = BoolExpr | NumExpr | StringExpr
           deriving (Show)

data Stmt = Empty
          | Body [Stmt]
          | Assign String Expr
		  | Declare Bool PsiType String Expr
		  | If BoolExpr Stmt Stmt
		  | While BoolExpr Stmt
		  | FnCall String [String]
		  | FnDecl String Effect [PsiType] PsiType
		  | FnImpl String [String] Stmt Stmt


