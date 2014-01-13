module Transicao where

data Transicao a e
    = TransVazia e e
    | Trans a e e
    deriving (Eq, Ord, Show)

origem, destino :: Transicao a e -> e

origem (TransVazia o _) = o
origem (Trans _ o _) = o

destino (TransVazia _ d) = d
destino (Trans _ _ d) = d

entrada :: Transicao a e -> Maybe a
entrada (Trans a _ _) = Just a
entrada _ = Nothing

mapT :: (e -> e) -> Transicao a e -> Transicao a e
mapT f (TransVazia o d) = TransVazia (f o) (f d)
mapT f (Trans e o d) = Trans e (f o) (f d)

vazia :: Transicao a e -> Bool
vazia (TransVazia _ _) = True
vazia _                = False