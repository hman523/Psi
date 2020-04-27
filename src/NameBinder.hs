module NameBinder where
import qualified Data.Map as Map
import Types
type Name = String
type FnDeclTable  = Map.Map String FnDecl
type FnImplTable  = Map.Map FnDecl FnImpl
type VarDeclTable = Map.Map String VarDecl
-- TODO figure out scoping for the var decl table
bindNames :: AST -> Either String (FnDeclTable, FnImplTable, VarDeclTable)
bindNames = undefined
