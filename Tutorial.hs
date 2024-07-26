import Data.List
import System.IO

sumonums = sum [1..1000]

madEx=mod 5 4
madEx2 = 5 `mod` 4
negNum=5 + (-4)
primenumbers=[3,5,7,11]
-- concat list
moreprimes=primenumbers ++ [13,17,23]
-- : is also possible to combine numbers into list 
combine = 10 : moreprimes
lencombine= length combine 

multiable = [ x*y | y<-[1..10], x<-[1..10]]