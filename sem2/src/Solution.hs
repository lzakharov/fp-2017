module Solution where

import Data.Either
import Types

-- typing environment
type Env = [(Symbol, Type)]

extend :: (Symbol, Type) -> Env -> Env
extend xt env = xt : env

lookupVar :: Symbol -> Env -> Either String Type
lookupVar x env = case lookup x env of
  Just t  -> Right t
  Nothing -> Left $ "variable" ++ show x ++ "not in scope"

typeOf :: Term -> Either String Type
typeOf = typeOf' []
  where
    typeOf' env t = case t of
      Sym x -> lookupVar x env
      Lam x ty1 t -> case typeOf' (extend (x, ty1) env) t of
        Right ty2 -> Right $ Fun ty1 ty2
        _         -> Left "type mismatch in lambda"
      App t1 t2 -> case ty1 of
        Right (Fun ty1' ty2') | Right ty1' == ty2 -> Right ty2'
                              | otherwise -> Left "type mismatch in arguments of operator App"
        _ -> Left "type mismatch in first term of application"
        where
          ty1 = typeOf' env t1
          ty2 = typeOf' env t2
      --
      Natural n -> if n >= 0 then Right Nat else Left "number is not natural"
      Add t1 t2 | ty1 /= Right Nat -> Left "first argument of operator Add must be a nutural number"
                | ty2 /= Right Nat -> Left "second argument of operator Add must be a nutural number"
                | otherwise        -> Right Nat
                where
                  ty1 = typeOf' env t1
                  ty2 = typeOf' env t2
      Mult t1 t2 | ty1 /= Right Nat -> Left "first argument of operator Mult must be a nutural number"
                 | ty2 /= Right Nat -> Left "second argument of operator Mult must be a nutural number"
                 | otherwise        -> Right Nat
                 where
                   ty1 = typeOf' env t1
                   ty2 = typeOf' env t2
      --
      Boolean _ -> Right Bool
      Not t | typeOf' env t /= Right Bool -> Left "argument of operator Not must be a boolean"
            | otherwise                 -> Right Bool
      And t1 t2 | ty1 /= Right Bool -> Left "first argument of operator And must be a boolean"
                | ty2 /= Right Bool -> Left "second argument of operator And must be a boolean"
                | otherwise         -> Right Bool
                where
                  ty1 = typeOf' env t1
                  ty2 = typeOf' env t2
      Or t1 t2 | ty1 /= Right Bool -> Left "first argument of operator Or must be a boolean"
               | ty2 /= Right Bool -> Left "second argument of operator Or must be a boolean"
               | otherwise         -> Right Bool
                 where
                   ty1 = typeOf' env t1
                   ty2 = typeOf' env t2
      Iff t1 t2 t3 | ty1 /= Right Bool   -> Left "guard of conditional not a boolean"
                   | ty2 /= ty3          -> Left "arms of conditional have different types"
                   | otherwise           -> Right Bool
                   where
                     ty1 = typeOf' env t1
                     ty2 = typeOf' env t2
                     ty3 = typeOf' env t3
      --
      Pair t1 t2 -> case typeOf' env t1 of
        Left _    -> Left "type mismatch in first element of pair"
        Right ty1 -> case typeOf' env t2 of
          Left _    -> Left "type mismatch in second element of pair"
          Right ty2 -> Right $ PairT ty1 ty2
      Fst t | isLeft ty -> Left "type mismatch in argument of operator Fst"
            | otherwise -> ty
            where
              ty = typeOf' env t
      Snd t | isLeft ty -> Left "type mismatch in argument of operator Snd"
            | otherwise -> ty
            where
              ty = typeOf' env t
      --
      Nil -> Right $ List Base
      IsNil t -> case typeOf' env t of
        Right (List _) -> Right Bool
        _              -> Left "argument of operator IsNil must be a list"
      Cons t1 t2  -> case typeOf' env t2 of
        Right (List ty2) -> case typeOf' env t1 of
          (Right ty1) | ty1 /= ty2 -> Left "first argument of operator Cons must have same type as the elements of the second argument"
                      | otherwise  -> Right $ List ty1
          _ -> Left "type mismatch in first argument of operator Cons"
        _ -> Left "second argument of operator Cons must be a list"
      Head t -> case typeOf' env t of
        Right (List ty) -> Right ty
        _               -> Left "argument of operator Head must be a list"
      Tail t -> case typeOf' env t of
        Right (List ty) -> Right ty
        _               -> Left "argument of operator Tail must be a list"
