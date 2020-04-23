-- "safe"
-- NoMonomorphismRestriction makes the type checker
-- more general
-- "unsafe" GeneralizedNewtypeDeriving
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE GADTs #-}

import Data.List
import Control.Monad.Cont
import Control.Monad.Except
import Control.Monad.Writer

main = putStrLn "Hello, world!"

product0 :: [Int] -> Int
product0 [] = 0
product0 (x:xs) = x * product xs

product1 = foldr (*) 1
product2 = foldl' (*) 1

-- Trees where data is at leaves and branches
-- data T a = L a | B a (T a) (T a)
--          deriving (Functor, Foldable)
-- T a is trees over some type a
-- T Int, T Bool

-- Trees where data is at leaves (lisp sexprs)
data T' a = L' a | B' (T' a) (T' a)
          deriving (Functor, Foldable)

-- Trees with just the shape
data T'' a = L'' | B'' (T'' a) (T'' a)
            deriving (Functor, Foldable)

-- t1 = B 2 (L 1) (L 3)

-- Let's make something more efficient

-- If 0 is encountered we can 0 immediately.
-- Still O(n), can be better if you have 0
-- sometimes and a massive tree.

-- Direct recursion
product' :: [Int] -> Int
product' [] = 0
product' (x:xs) | x == 0    = 0
                | otherwise = x * product' xs

-- Sure it works but here's how it works
{-
product' [1,2,0,4,5]
=> 1 * product' [2,0,4,5]
=> 1 * (2 * product' [0,4,5])
-- the continuation of product' [0,4,5] is
-- 1 * (2 * _) (the return context)
=> 1 * (2 * 0)
=> 1 * 0
=> 0
-}

-- Some C++ solutions
-- You could throw an exception and catch it!
-- setjmp/longjmp

-- The solution we need is a continuation
productK' :: Num t => [t] -> (t -> p) -> p
productK' [] k = k 1
productK' (x:xs) k = productK' xs (\n -> k (x * n))

-- We make the continuation context explicit

{-
productK [1..3] show
=> productK [2..3] (\n -> show (1 * n))
=> productK [3..3] (\n -> prev (2 * n))
=> productK [] (prev 1)
=> ... 6
-}

-- Exiting early
productK :: (Eq t, Num t, Num p) =>
            [t] -> (t -> p) -> p
productK [] k = k 1
productK (x:xs) k | x == 0    = 0
                  | otherwise = 
                      productK xs (\n -> k (x * n))

product4 x = productK x id

-- Let's make it better with a monad
productC :: (Num a, Eq a) => [a] -> Cont r a
productC []     = pure 1
productC (x:xs) | x == 0 = callCC (\k -> pure 0)
                | otherwise = do n <- productC xs
                                 pure (x * n)

{-
Writing an interpreter
- Direct recursion (problems with error handling)
- Monadic (harder)
- GADTs (static typed guarantees, no type tags, complexity)
-}
-- GADTs (weak form of dependent types)
data E a where
   LitB' :: Bool -> E Bool
   LitI' :: Int -> E Int
   If' :: E Bool -> E a -> E a -> E a
   Add' :: E Int -> E Int -> E Int

ex' :: E Bool
ex' = If' (LitB' False)
          (LitB' True)
          (If' (LitB' True) (LitB' False) (LitB' True))

eval' :: E a -> a
eval' (LitI' i) = i
eval' (LitB' b) = b
eval' (If' p c a) = if eval' p then eval' c else eval' a
eval' (Add' l r) = eval' l + eval' r

-- Expr
data Expr = If Expr Expr Expr
          | Add Expr Expr
          | LitB Bool
          | LitI Int
          deriving Show
-- Types
data Type = II | BB deriving (Show, Eq)
-- Values
data V = I Int | B Bool deriving Show

-- Expr, Value, Type
ex2 = If (LitB False) (LitI 1) (Add (LitI 3) (LitI 4))
ex3 = If (LitB False) (LitI 1) (Add (LitB False) (LitI 4))
ex4 = If (LitI 1) (LitI 1) (Add (LitB False) (LitI 4))

-- eval :: Expr -> Except
eval :: Expr -> Except String V
eval (LitI i) = pure (I i)
eval (Add l r) = do n <- eval l
                    m <- eval r
                    case (n, m) of
                      (I n, I m) -> pure (I (n + m))
                      _ -> throwError "expected int"
eval (If p c a) = do b <- eval p 
                     case b of
                        B b -> if b then eval c else eval a
                        _ -> throwError "expected bool"

eval (LitB b) = pure (B b)

-- Transparent
type Money = Int
-- Opaque
newtype Money' = Money' { getMoney' :: Int }
-- Money' . getMoney' = getMoney' . Money' = id
-- id . id = id . id = id

-- typeCheck :: Tree Expr -> 
-- typeCheck :: Expr -> Type
-- It returns a type, possibly raising an exception of type
-- string

typeCheck :: Expr -> Except String Type
typeCheck (LitI _) = pure II
typeCheck (LitB _) = pure BB
typeCheck (Add l r) = do expect II l
                         expect II r
                         pure II
typeCheck (If p c a) = do expect BB p
                          x <- typeCheck c
                          y <- typeCheck a
                          guard (x == y)
                          pure x

-- expect :: Type -> Expr -> Except String Type
expect expected e = do got <- typeCheck e
                       guard (expected == got)

check :: Expr -> Either String Type
check = runExcept . typeCheck
-- <id> :: (pure|impure|IO|RS|WS|Mem) [(int, int) -> (int)];
-- let x = 5 in let x = 3 in x

-- If True then c else a ==> c
-- If True then c else a ==> a

-- Also known as "partial evaluation"
optimize :: Expr -> Expr
optimize (If (LitB True) c a) = c
optimize (If (LitB False) c a) = a
optimize (Add l r) = Add (optimize l) (optimize r)

traceList :: Show a => [a] -> Writer String ()
traceList [] = tell " null "
traceList (x:xs) = do tell (show x)
                      traceList xs

-- execWriter $ traceList [1..5]
pretty :: Expr -> Writer String ()
pretty (LitI i) = tell (show i)
pretty (LitB b) = tell (show b)
pretty (Add l r) = do tell "("
                      pretty l
                      tell "+"
                      pretty r
                      tell ")"
pretty (If a b c) = do tell "("
                       pretty a 
                       tell "?"
                       pretty b
                       tell ":"
                       pretty c 
                       tell ")"
-- local -- modify the reader state temporarily in an action
-- 
