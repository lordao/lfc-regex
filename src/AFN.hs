module AFN where
import Data.Set (Set)
import qualified Data.Set as S
import Transicao 

data AFN a = AFN { estados     :: Set Int
                 , estadoAtual :: Set Int
                 , aceitacao   :: Set Int
                 , inicio      :: Int
                 , transicoes  :: Set (Transicao a Int)
                 }
             deriving (Eq,Show)

concatenar, union :: Ord a => AFN a -> AFN a -> AFN a
concatenar m1 m2 = m1 { estados    = estados m1 `S.union` estados m2'
                      , aceitacao  = aceitacao m2'
                      , transicoes = transicoes m1 `S.union` transicoes m2'
                      }
    where es1Size = (\n -> n-1) . S.size . estados $ m1
          m2' = m2 { estados    = S.map (+es1Size) . estados $ m2
                   , aceitacao  = S.map (+es1Size) . aceitacao $ m2
                   , transicoes = S.map (mapT (+es1Size)) $ transicoes m2
                   }

union m1 m2 = m3{estadoAtual = fechoVazio m3}
    where m1' = atualizarAFN 1 m1
          m2' = atualizarAFN 1 $ ajustarIndices m1 m2
          m3  = AFN { estados     = S.insert 0 $ estados m1' `S.union` estados m2'
                    , estadoAtual = fechoVazio m3  
                    , aceitacao   = aceitacao m1' `S.union` aceitacao m2'
                    , inicio      = 0
                    , transicoes  = S.fromList [TransVazia 0 $ inicio m1', TransVazia 0 $ inicio m2'] 
                                    `S.union` transicoes m1' 
                                    `S.union` transicoes m2' 
                    }

{-- Algoritmo simples para implementar o fecho de Kleene (estrela) é o seguinte:
    1. Some 1 aos identificadores de todos os estados e transações do Autômato FN; 
    2. Crie um novo estado inicial, chame-o 0;
    3. Para cada estado final do Autômato, crie uma transação vazia para 0; 
    4. Torne 0 o único estado final; --}
estrela :: Ord a => AFN a -> AFN a
estrela m = m'
    where m' = m { estados     = S.insert 0 . S.map (+1) $ estados m
                 , estadoAtual = fechoVazio m'
                 , aceitacao   = S.singleton 0
                 , transicoes  = S.insert (TransVazia 0 1) . S.map (mapT (+1)) $ transicoes m
                 }

-- Autômato que reconhece a expressão 'E'.
zero :: Ord a => AFN a
zero = AFN { estados = es
           , aceitacao = S.singleton 1
           , estadoAtual = es
           , inicio = 0
           , transicoes = S.singleton $ TransVazia 0 1
           }
   where es = S.fromList [0,1]

-- Cria um AFN que reconhece a linguagem cuja única palavra é o literal passado.   
single :: Ord a => a -> AFN a
single a = AFN { estados     = S.fromAscList [0,1]
               , aceitacao   = S.singleton 1
               , estadoAtual = S.singleton 0
               , inicio      = 0
               , transicoes  = S.singleton $ Trans a 0 1
               }

executar :: [a] -> AFN a -> (Bool,[Int])
executar = undefined

delta :: Ord a => AFN a -> Int -> a -> Set Int
delta m q i = S.map destino . S.filter cond . transicoes $ m
    where cond (TransVazia _ _) = True
          cond (Trans i' q' _)  = i == i' && q == q'

ajustarIndices :: Ord a => AFN a -> AFN a -> AFN a
ajustarIndices m1 m2 = atualizarAFN s1 m2
    where s1 = S.size $ estados m1

atualizarAFN :: Ord a => Int -> AFN a -> AFN a
atualizarAFN n m = m { estados    = es
                     , aceitacao  = fs
                     , transicoes = ts
                     , inicio     = n
                     }
    where es = S.map (+n) $ estados m
          fs = S.map (+n) $ aceitacao m
          ts = S.map (mapT (+n)) $ transicoes m

fechoVazio :: Ord a => AFN a -> Set Int
fechoVazio m = S.map destino $ S.filter (\t -> vazia t && origem t == e) ts
    where e  = inicio m
          ts = transicoes m

