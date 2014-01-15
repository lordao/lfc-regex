module Regex where

--Para representar expressões regulares teremos de criar um tipo algébrico.
--O tipo "Regex a" será utilizado.

data Regex a
{-
 - Quanto aos construtores, precisaremos de 5, um para cada tipo possível de expressão regular:
 -  |+ 'E', ou palavra vazia;
 -  |+ 'a', um literal qualquer;
 -  |+ r1 . r2 (ou r1r2), a concatenação de duas expressões regulares;
 -  |+ r1 + r2, união de duas expressões regulares;
 -  |+ r*, a operação estrela, ou fecho de Kleene.
 -}
   = Vazia
   | Literal a
   | Regex a :+ Regex a
   | Regex a :. Regex a
   | Kleene (Regex a)
   deriving (Eq, Read, Show)
{- Para respeitar as regras de prioridade entre as operações de Concatenação e União,
 - mudamos a precedência dos operadores para valores arbitrários, desde que a precedência
 - de (:.) seja maior do que a de (:+).
 -}
infixl 5 :.
infixl 4 :+

parse :: String -> Regex Char
parse str = undefined
