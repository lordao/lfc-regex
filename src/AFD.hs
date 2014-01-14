module AFD where
import AFN (AFN)
import qualified AFN
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
