--	remove duplicates from a list
rd l = foldl (\acc e -> if (e `elem` acc) then acc else acc ++ [e]) [] l

--	make pairs (e, count(e))
--	[1,1,1,2,2,3] -> [(1,3), (2,2), (3,1)]

aux x l = foldl (\(c, l) e -> if (x == e) then (1 + c, l) else (c, l ++ [e])) (0, []) l

count x l = fst (aux x l)
listWithout x l = snd (aux x l)

mp [] = []
mp (e:l) = (count e (e:l), e) : (mp (listWithout e l))

trim acc _ [] = (acc, [])
trim acc e (h:t) = if (e == h) then (trim (acc+1) e t) else (acc, (h:t))

mp' [] = []
mp' (e:l) = (fst tpl, e) : (mp' (snd tpl)) where tpl = trim 0 e (e:l)


--	replicate
copy e 0 = []
copy e n = e : (copy e (n-1))

repl [] _ = []
repl (e:l) n = (copy e n) ++ (repl l n)


-- zipWith with fold
zw op l1 l2 = let len = (length l1 - 1) in
                foldr (\f acc -> (f (l2 !! (len - length acc)):acc)) [] l1' 
                where l1' = map (\x -> op x) l1
