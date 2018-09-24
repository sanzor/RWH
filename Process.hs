module Process where
import Split

    aaR::(String->[String])->String->String->IO [String]
    aaR func input output=do
        text<- readFile ((??)input defaultIn) 
        putStrLn 
        where
            defaultIn="input.txt"
            defaultOut="output.txt"

-- listCols::String->[String]
-- listCols [] = []
-- listCols text=go text [] where
--     go  (col:cols) = do
    
replicate::Int->[Int]->[[Int]]
replicate 1 x=x
replicate factor (x:xs)= go factor [] (x:xs) where
                         go factor ls (x:xs) = repl factor x:go factor xs
                         repl 1 t=t
                         repl times nr=nr:repl (times-1) nr
                         
               
