module Main (main) where

main :: IO ()
main = print("aa")


{-
import JsonLibrary

-}

--class Functor f where 
  --  fmap:: (a->b) -> fa -> fb

--instance Functor [] where 
    --map:: (a -> b)-> [a] -> [b]
    --fmap :: (a->b) -> [a] -> [b]
    --fmap = map

data Btree a = Leaf a | Node a (Btree a) (Btree a)



inc::(Functor f, Num a) => f a -> f a
inc = fmap(+1)

{-
instance Functor Btree where
  --fmap:: (a->b) -> Btree a -> Btree b
  fmap g(Leaf a) = Leaf(g a)
  fmap g(Node a l r) = Node(g a) (fmap g l) (fmap g r)

sqre :: [Int] -> [Int]
sqre map(\x -> x * x)

duplicateChar :: [Char] -> [Char]
duplicateChar (x:xs) = x :x : duplicateChar xs


duplicateChar :: [Char] -> [Char]
duplicateChar = concatMap(\x -> [x,x])
-}

sqre:: (Functor f) => f Int -> f Int
sqre = fmap(\x -> x*x)


duplicateChar :: [Char] -> [Char]
duplicateChar = fmap(\x -> x : [x,x])

