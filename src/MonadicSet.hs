module MonadicSet (
    module Data.Set,
    (>>=),
    (>>),
    join
) where

import Data.Set
import Prelude hiding ((>>=), (>>), foldr, map)

(>>=) :: (Ord a, Ord b) => Set a -> (a -> Set b) -> Set b
s >>= f = foldr union empty . map f $ s
infixl 1 >>=

(>>) :: (Ord a, Ord b) => Set a -> Set b -> Set b
s1 >> s2 = s1 >>= \_ -> s2
infixl 1 >>

join :: Ord a => Set (Set a) -> Set a
join s = s >>= id