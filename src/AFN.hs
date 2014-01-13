module AFN where
import Data.List (foldl1')
import Data.Set (Set)
import qualified Data.Set as S
import Regex
import Transicao

data AFN a = AFN { estados       :: Set Int
                 , estadosAtuais :: Set Int
                 , aceitacao     :: Set Int
                 , inicio        :: Int
                 , transicoes    :: Set (Transicao a Int)
                 }
             deriving (Eq,Show)

buildAFN :: Regex Char -> (Set Char, AFN Char)
buildAFN Vazia       = (S.singleton 'E', zero)
buildAFN (Literal l) = (S.singleton l, single l)
buildAFN (Kleene r)  = let (a,m) = buildAFN r in (a,estrela m)
buildAFN r = let ((a1,m1),(a2,m2),f) = case r of
                                            r1 :. r2 -> (buildAFN r1,buildAFN r2, concatenar)
                                            r1 :+ r2 -> (buildAFN r1,buildAFN r2, union)
                                            _        -> error "Não vai acontecer =)"
              in (a1 `S.union` a2, f m1 m2)

concatenar, union :: Ord a => AFN a -> AFN a -> AFN a
concatenar m1 m2 = m1' { estadosAtuais = fechoVazio m1' }
    where m2' = ajustarIndices m1 m2
          transVazia i q = TransVazia q i
          m1' = m1 { estados = estados m1 `S.union` estados m2'
                   , aceitacao = aceitacao m2'
                   , transicoes = S.map (transVazia (inicio m2')) (aceitacao m1)
                                 `S.union` transicoes m1 `S.union` transicoes m2'
                   }

union m1 m2 = m3{estadosAtuais = fechoVazio m3}
    where m1' = atualizarAFN 1 m1
          m2' = atualizarAFN 1 $ ajustarIndices m1' m2
          m3  = AFN { estados       = S.insert 0 $ estados m1' `S.union` estados m2'
                    , estadosAtuais = fechoVazio m3
                    , aceitacao     = aceitacao m1' `S.union` aceitacao m2'
                    , inicio        = 0
                    , transicoes    = S.fromList [TransVazia 0 $ inicio m1', TransVazia 0 $ inicio m2']
                                      `S.union` transicoes m1'
                                      `S.union` transicoes m2'
                    }

{- Algoritmo simples para implementar o fecho de Kleene (estrela) é o seguinte:
    1. Some 1 aos identificadores de todos os estados e transações do Autômato FN;
    2. Crie um novo estado inicial, chame-o 0;
    3. Para cada estado final do Autômato, crie uma transação vazia para 0;
    4. Torne 0 o único estado final; -}
estrela :: Ord a => AFN a -> AFN a
estrela m = m'
    where m' = m { estados       = S.insert 0 . S.map (+1) $ estados m
                 , estadosAtuais = fechoVazio m'
                 , aceitacao     = S.singleton 0
                 , transicoes    = S.insert (TransVazia 0 1) . S.map (mapT (+1)) $ transicoes m
                 }

-- Autômato que reconhece a expressão 'E'.
zero :: Ord a => AFN a
zero = AFN { estados = es
           , aceitacao = es
           , estadosAtuais = es
           , inicio = 0
           , transicoes = S.empty
           }
   where es = S.singleton 0

--Autômato para a linguagem vazia
empty :: AFN a
empty = AFN { estados       = S.singleton 0
            , aceitacao     = S.empty
            , estadosAtuais = S.singleton 0
            , inicio        = 0
            , transicoes    = S.empty
            }

-- Cria um AFN que reconhece a linguagem cuja única palavra é o literal passado.
single :: Ord a => a -> AFN a
single a = AFN { estados       = S.fromAscList [0,1]
               , aceitacao     = S.singleton 1
               , estadosAtuais = S.singleton 0
               , inicio        = 0
               , transicoes    = S.singleton $ Trans a 0 1
               }

aceita :: Ord a => AFN a -> [a] -> Bool
aceita m =  any (`S.member` aceitacao m) . S.toList . executar m

executar :: Ord a => AFN a -> [a] -> Set Int
executar m = estadosAtuais . foldl passo m

passo :: Ord a => AFN a -> a -> AFN a
passo m i = m { estadosAtuais = foldl1' S.union . map (delta m i) . S.toList $ estadosAtuais m }

delta :: Ord a => AFN a -> a -> Int -> Set Int
delta m i q = S.map destino . S.filter cond $ transicoes m
    where cond (TransVazia _ _) = True
          cond (Trans i' q' _)  = i == i' && q == q'

ajustarIndices :: Ord a => AFN a -> AFN a -> AFN a
ajustarIndices m1 = atualizarAFN s1
    where s1 = S.size $ estados m1

atualizarAFN :: Ord a => Int -> AFN a -> AFN a
atualizarAFN n m = m' { estadosAtuais = fechoVazio m' }
    where es = S.map (+n) $ estados m
          fs = S.map (+n) $ aceitacao m
          ts = S.map (mapT (+n)) $ transicoes m
          m' = m { estados    = es
                 , aceitacao  = fs
                 , transicoes = ts
                 , inicio     = inicio m + n
                 }

fechoVazio :: Ord a => AFN a -> Set Int
fechoVazio m = S.map destino $ S.filter (\t -> vazia t && origem t == e) ts
    where e  = inicio m
          ts = transicoes m

