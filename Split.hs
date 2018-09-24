module Split where

import System.Environment
import Data.List
import Data.Maybe


process::Int->[a]->[[a]]
process nr ls=go nr ls [] where
              go _ [] _ =[]
              go nr (x:xs) out=repl nr x:go nr xs out
              repl 0 num=[num]
              repl nr num=num:repl (nr-1) num


manF::[a]->[a]
manF (_:x:xs)=x: manF xs
manF [x]=[x]
manF [] = []


-- multiple::Int->Int Bool Char
-- multiple x =get x rest  where
--               get::Int->Bool Char Int
--               get el = el>0>>=True 'a' el


mymap::(a->b)->[a]->[b]
mymap _ [] =[]
mymap f (x:xs)=f x : mymap f xs
