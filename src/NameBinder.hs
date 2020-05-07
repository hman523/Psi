module NameBinder where
import qualified Data.Map                      as Map
import           Types
import           Data.Maybe
import           Data.List
import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Identity

type BinderT e m a = StateT Tables (ExceptT e m) a

bindNames :: AST -> Either BindError Tables
bindNames x = snd <$> runBind (bindStmt x) bindBuiltIns

runBindT :: (Monad m) => BinderT e m a -> Tables -> m (Either e (a, Tables))
runBindT m = runExceptT . runStateT m

type Binder e a = BinderT e Identity a
runBind m = runIdentity . runBindT m

bindStmt :: Stmt -> Binder BindError ()
bindStmt (FuncDecl f@(name, _, _, _)) = do
  (fndecltab, x, y) <- get
  when (Map.member name fndecltab) $ throwError (RedeclaredFunction f)
  put (Map.insert name f fndecltab, x, y)
bindStmt (FuncImpl f@(name, _, _)) = do
  (fndecltab, fnimpltab, y) <- get
  if Map.member name fndecltab
    then if not (Map.member name fnimpltab)
      then put (fndecltab, Map.insert name f fnimpltab, y)
      else throwError (RedefinedFunction f)
    else throwError (UndeclaredFunction f)
bindStmt (Body b) = do
  startState <- get
  put (enterBody startState)
  mapM_ bindStmt b
  endState <- get
  put (exitBody endState)
bindStmt _ = pure ()


enterBody :: Tables -> Tables
enterBody = undefined

exitBody :: Tables -> Tables
exitBody = undefined

getParents :: Tables -> Int -> [Int]
getParents (_, _, (_, pt)) x = pt !! x

-- This functions comfirms all function delcarations have implementations
confirmDecls :: Tables -> Bool
confirmDecls (x, y, _) = not (null (Map.keys x \\ Map.keys y))

bindBuiltIns :: Tables
bindBuiltIns =
  ( Map.fromList [("main", ("main", Impure, [], VoidT))]
  , Map.empty
  , ([Map.empty], [[0]])
  )
