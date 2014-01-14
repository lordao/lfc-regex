module Conversor where
import AFD (AFD)
import AFN
import MonadicSet (Set)
import qualified MonadicSet as S
import Transicao

afn2afd :: Ord a => Set a -> AFN a -> AFD a
afn2afd sigma m = undefined
    where es  = estadosDet sigma m
          ies = indicesEstados es
          ts  = transicoesDet (transicoes m) ies 
          

indicesEstados :: Set [Int] -> [(Int,[Int])]
indicesEstados = undefined

transicoesDet :: Ord a => Set (Transicao a Int) -> [(Int,[Int])] -> Set (Transicao a Int)
transicoesDet ts qs = undefined 

estadosDet :: Ord a => Set a -> AFN a -> Set [Int]
estadosDet sigma = estadosDet' S.empty
    where estadosDet' qs m
              | qs == qs' = qs
              | otherwise = estadosDet' ms m
                  where qs' = S.insert (S.toList $ estadosAtuais m) qs
                        ms  = sigma S.>>= estadosDet' qs' . passo m

-- Exemplos
m1 = AFN { estados = S.fromList [1 .. 4]
        , estadosAtuais = S.insert 1 $ fechoVazio m1
        , aceitacao = S.singleton 4
        , inicio = 1
        , transicoes = S.fromList [ Trans '0' 1 1
                                  , Trans '1' 1 1

                                  , Trans '1' 1 2
                                  
                                  , Trans '0' 2 3
                                  , Trans '1' 2 3
                                  
                                  , Trans '0' 3 4
                                  , Trans '1' 3 4
                                  ]
        }

m2 = AFN { estados = S.fromList [1 .. 3]
        , estadosAtuais = S.insert (inicio m2) $ fechoVazio m2
        , aceitacao = S.singleton 1
        , inicio = 1
        , transicoes = S.fromList [ TransVazia 1 3
                                  , Trans '1' 1 2
                                  
                                  , Trans '0' 2 2
                                  , Trans '0' 2 3
                                  , Trans '1' 2 3
                                  
                                  , Trans '0' 3 1
                                  ]
        }