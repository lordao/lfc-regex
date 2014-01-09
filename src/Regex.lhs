>module Regex where
>import Data.Set (Set)
>import qualified Data.Set as S

Para representar expressões regulares teremos de criar um tipo algébrico.
O tipo "Regex a" será utilizado.

>data Regex a

Quanto aos construtores, precisaremos de 5, um para cada tipo possível de expressão regular:
    - 'E', ou palavra vazia;
    - 'a', um literal qualquer;
    - r1 . r2 (ou r1r2), a concatenação de duas expressões regulares;
    - r1 + r2, união de duas expressões regulares;
    -  r*, a operação estrela, ou fecho de Kleene.

>   = Vazia
>   | Literal a
>   | Regex a :+ Regex a
>   | Regex a :. Regex a
>   | Kleene (Regex a)
>   deriving (Eq,Show)

Para respeitar as regras de prioridade entre as operações de Concatenação e União, 
mudamos a precedência dos operadores para valores arbitrários, desde que a precedência
de (:.) seja maior do que a de (:+). 

>infixr 5 :.
>infixr 4 :+

>alfabeto :: Ord a => Regex a -> Set a
>alfabeto Vazia       = S.empty
>alfabeto (Literal a)  = S.singleton a
>alfabeto (r1 :. r2)  = alfabeto r1 `S.union` alfabeto r2
>alfabeto (r1 :+ r2) = alfabeto r1 `S.union` alfabeto r2
>alfabeto (Kleene r) = alfabeto r