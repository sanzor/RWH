module Ex98 where
import Data.Char
import Data.List

afold::(a->b->b)->b->[a]->b
afold tsf accu (x:xs)=afold tsf (tsf x accu) xs
afold _ accu []=accu


bfold::(b->a->b)->b->[a]->b
bfold tsf accu (x:xs)=bfold tsf (tsf accu x) xs

filt::(a->Bool)->[a]->[a]
filt p []=[]
filt p (x:xs)=if p x then x : filt p xs else filt p xs

mymap::(a->b)->[a]->[b]
mymap tf []=[]
mymap tf (x:xs)=tf x:mymap tf xs

-- myfoldl::(b->a->b)->b->[a]->b
-- myfoldl aggr accu ls=foldr aggr accu ls

conc::[a]->[a]
conc xs@(x1:xs')=foldr (++) [] xs

tkw::(a->Bool)->[a]->[a]
tkw p []=[]
tkw p ls=go p ls [] where
         go p [] accu=accu
         go p (x:xs) accu | p x =go p xs x:accu
                          | otherwise =accu 


