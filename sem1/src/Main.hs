module Main where

newtype Symbol = Symbol { unSymbol :: String } deriving (Eq,Show,Read)

data TermS = SymS Symbol        -- x
           | LamS Symbol TermS  -- \x -> t
           | AppS TermS TermS   -- t1 t2
           deriving (Eq,Show,Read)

data TermI = SymI Int
           | LamI TermI
           | AppI TermI TermI
           deriving (Eq,Show,Read)

--  getIndex transforms a lambda term into it's de Bruijn representation
getIndex :: (Eq a) => a -> [a] -> Int
getIndex a xs = getIndex' 0 (reverse xs)
  where
    getIndex' i [] = i
    getIndex' i (x:xs) | a == x    = i
                       | otherwise = getIndex' (i + 1) xs

toTermI :: TermS -> TermI
toTermI t = toTermI' [] t
  where
    toTermI' g (SymS x) = SymI (getIndex x g)
    toTermI' g (LamS x t) = LamI (toTermI' (g ++ [x]) t)
    toTermI' g (AppS t1 t2) = AppI (toTermI' g t1) (toTermI' g t2)

-- let
sym x = SymS (Symbol x)
lam x t = LamS (Symbol x) t
app t1 t2 = AppS t1 t2

main :: IO ()
main = do
  putStrLn "Семестровая работа №1"
