module TestRank
where

import           Data.Map        (Map)
import qualified Data.Map        as M
import           Data.Set        (Set)
import qualified Data.Set        as S
import qualified PackageRank     as PR

edges = [ ('A', "BCD"), ('B', "D"), ('C', "E"), ('E', "F") ]

rel = PR.fromList $ PR.dagListToList edges

test1 = PR.transClosure rel

