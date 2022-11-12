

-- exercise 2 in https://rpeszek.github.io/posts/2022-11-07-empirical-programming.html


partitionEithers :: [Either a b] -> ([a], [b]) 
partitionEithers [] = ([], [])                     -- (p0)
partitionEithers (Left a: es') = (a: ra, as)       -- (p1.1)
    where (ra, as) = partitionEithers es'
partitionEithers (Right b: es') = (ra', b: as')    -- (p1.2)
    where (ra', as') = partitionEithers es'


len :: [a] -> Int
len [] = 0               -- (l0)
len (a: as) = 1 + len as -- (l1)

{-
Property:
len es = len as + len cs
 where (as, cs) = partitionSecondEithers es

Proof:
(0) induction base: es = [] 

from (l0): len es = 0   (0a)
from (p0): as = [] and cs = []  
from (l0): len as = 0 and len cs = 0 (0b)
combining (0a) (0b): len es = len as + cs = 0 

(1) inductive case es = (Left a) : es' (1.1) or es = (Right b) : es' (1.2) are only possible construtors for non-empty es.

Inductive assumption: 
    len es' = len as' + len cs' where (as', cs') = partitionSecondEithers es' (IA)

from (l1): len es = len es' + 1 (1a)
case 1.1: 
  from (p1.1,l1): len as = 1 + len as'
  from (p1.1):  len cs = len cs'
  together: len as + len cs = len as' + len cs' + 1  (1.1b)
case 1.2:
  from (p1.2): len as = len as'
  from (p1.2, l1): len cs = 1 + len cs' 
  together: len as + len cs = len as' + len cs' + 1 (1.2b)
combining (1a and either 1.1b or 1.2b and IA):
      (1a)          (IA)               (1.1b or 1.2b)
len es = len es' + 1 = len as' + len cs' + 1 = len as + len cs
-}



