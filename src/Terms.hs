module Terms where

import qualified Data.Map as Map
import Data.String

type ConstrName = String
data Constr = Constr ConstrName [Type]

type IndName = String
type Inductive = [Constr]
type IDefs = Map.Map IndName Inductive

type TName = String
type TDefs = Map.Map TName Type
-- TVar is for local definitions, TName for global definitions
data Type = TVar Int
          | TInd IndName
          | TName TName
          | TFun Type Type
          | TInt
          | TString

data BinOp = Plus
  deriving (Show, Eq)

type EName = String
type EDefs = Map.Map EName Expr

data Expr = EVar Int
          | EConstr ConstrName [Expr]
          | EName EName
          | EAbs Expr
          | EApp Expr Expr
          | EInt Int
          | EString String
          | EBinOp BinOp Expr Expr
          -- IndName is the name of the inductive type of the match
          -- Each branch contains the branch body and the number of
          -- bound variables
          | EMatch IndName Expr (Map.Map ConstrName (Int, Expr))
  deriving (Show, Eq)

subst :: Int -> Expr -> Expr -> Expr
subst d x (EVar n)
  | d == n = x
  | d > n = EVar n
  | otherwise = EVar (n - 1)
subst d x (EConstr n l) = EConstr n (fmap (subst d x) l)
subst d x (EAbs b) = EAbs (subst (d + 1) x b)
subst d x (EApp f y) = EApp (subst d x f) (subst d x y)
subst d x (EBinOp b e1 e2) = EBinOp b (subst d x e1) (subst d x e2)
subst d x (EMatch i e m) =
  EMatch i (subst d x e)
  (fmap (\(k, e) -> (k, subst (d + k) x e)) m)
subst d x e = e

multisubst :: Int -> [Expr] -> Expr -> Expr
multisubst d [] b = b
multisubst d (x:xs) b = multisubst (d - 1) xs (subst d x b)

eval :: EDefs -> Expr -> Expr
eval c (EVar v) = error $ "Free variable " ++ show v ++ " evaluated"
eval c (EName n) = eval c (c Map.! n)
eval c (EApp f a) =
  case eval c f of
    EAbs b -> eval c (subst 0 a b)
    _ -> error "function does not reduce to lambda"
eval c (EBinOp b e1 e2) =
  case b of
    Plus ->
      case (eval c e1, eval c e2) of
        (EInt i1, EInt i2) -> EInt (i1 + i2)
        _ -> error "int does not reduce to constant"
eval c (EMatch _ x m) =
  case eval c x of
    EConstr n l -> eval c $ multisubst (length l - 1) l (snd $ m Map.! n)
    _ -> error "matched term does not reduce to constr"
eval c x = x
