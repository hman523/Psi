module NameBinder where
import qualified Data.Map                      as Map
import           Types
import           Data.Maybe
import           Data.List

bindNames :: AST -> Either [BindError] Tables
bindNames = undefined

bindStmt :: Stmt -> Tables -> Either BindError Tables
bindStmt Empty tabs = Right tabs
bindStmt (FuncDecl f@(name, _, _, _)) (fndecltab, x, y) =
  if Map.member name fndecltab
    then Left (RedeclaredFunction f)
    else Right (Map.insert name f fndecltab, x, y)
bindStmt (FuncImpl f@(name, _, _)) (fndecltab, fnimpltab, y) =
  if Map.member name fndecltab
    then if not (Map.member name fnimpltab)
      then Right (fndecltab, Map.insert name f fnimpltab, y)
      else Left $ RedefinedFunction f
    else Left $ UndeclaredFunction f

bindStmt _ tabs = Right tabs


-- This functions comfirms all function delcarations have implementations
confirmDecls :: Tables -> Bool
confirmDecls (x, y, _) = not (null (Map.keys x \\ Map.keys y))

bindBuiltIns :: Tables
bindBuiltIns =
  (Map.fromList [("main", ("main", Impure, [], VoidT))], Map.empty, Map.empty)
