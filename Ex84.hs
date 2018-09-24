module Ex84 where

    safeHead::[a]->Maybe a
    safeHead []=Nothing
    safeHead (x:xs)=Just x

    safeTail::[a]->Maybe[a]
    safeTail []=Nothing
    safeTail (x:xs)=Just xs

    safeInit::[a]->Maybe [a]
    safeInit (x:xs)=Just (go x xs) where
                   go _ [] =[]
                   go x (x2:xs)=x: go x2 xs

    safeLast::[a]->Maybe a
    safeLast []=Nothing
    safeLast (x:xs)=Just (go x xs) where
                          go x [] = x
                          go x (x2:xs)=go x2 xs

   
    
    -- splt::(a->Bool)->[a]->[[a]]
    -- splt p []=[]
    -- splt p (x:xs)=go p [] (x:xs) where
    --               go p rez []=rez
    --               go p rez ls=rez:process . brk $  ls [] where
    --               process  (x,[])=rez:x
    --               process ([],x)=go p rez x
                 

    -- brk::(a->Bool)->[a]-> ([a],[a])
    -- brk p []= ([],[])
    -- brk p (x:xs)=go p ([],x:xs) where 
    --              go p (accu,[])= (accu,[])
    --              go p (accu,x:xs)|not (p x) =(accu,xs)
    --                              |otherwise = go p (x:accu,xs)
                

                          
    fm::(a->Bool)->[a]->[[a]]
    fm p []=[]
    fm p xs=let (f,l) = span p xs
                (f',l') =break p xs in
                 if null f then fm p l' else f:fm p l'
                                                                    
                             
                                               
                             