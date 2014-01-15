module AFD where
import Data.Set (Set)
import qualified Data.Set as S
import Transicao

data AFD a = AFD { estados     :: Set Int
                 , aceitacao   :: Set Int
                 , estadoAtual :: Int
                 , inicio      :: Int
                 , transicoes  :: Set (Transicao a Int)
                 }
             deriving (Eq,Show)

aceita ::  Ord a => AFD a -> [a] -> Bool
aceita m = fst . executar m

executar :: Ord a => AFD a -> [a] -> (Bool, AFD a)
executar m is = (q `S.member` aceitacao m', m')
    where m' = foldl passo m is
          q  = estadoAtual m'

passo :: Ord a => AFD a -> a -> AFD a
passo m i = m { estadoAtual = destino t }
    where q = estadoAtual m
          t = head . S.toList . S.filter (\(Trans i' q' _) -> i' == i && q' == q) $ transicoes m