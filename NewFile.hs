polynome :: Int -> [Int] -> Int
polynome x []           = 0
polynome x (a:as)       = a + (x *(polynome (x*x) as))

polynome' :: Int -> [Int] -> Int
polynome' x []           = 0
polynome' x (a:as)       = foldl (+) a (map (*x) as)

-- LIST COMPREHENSION


add35 :: Int -> Int
add35 0     = 0
add35 n     | (n-1) `mod` 3 == 0        = n-1 + (add35 (n-1))
            | (n-1) `mod` 5 == 0        = n-1 + (add35 (n-1))
            | otherwise                 = add35 (n-1)
            
addks :: Int -> [Int] -> Int
addks 0 ks  = 0
addks n ks  | foldl (||) False (map (== 0) (map ((n-1) `mod`) ks))  = n-1 + (addks (n-1) ks)
            | otherwise                                             = addks (n-1) ks
            



data Tree = Node Int Tree Tree | Leaf



isBalanced :: Tree -> (Bool, Int)
isBalanced Leaf             = (True, 1)
isBalanced (Tree x t1 t2)   = (b, l)
    where 
        b = (fst (isBalanced t1)) && (fst (isBalanced t2)) && (d > -2) && (d < 2)
        d = (snd (isBalanced t1)) - (snd (isBalanced t2)))
        l = (maximum ((snd (isBalanced t1)) : [snd (isBalanced t2)])) + 1




isomorphic :: Tree -> Tree -> Bool
isomorphic Leaf Leaf                            = True
isomorphic (Tree x1 t11 t21) (Tree x2 t21 t22)  = (isomorphic t11 t21) && (isomorphic t12 t22) 
isomorphic _ _                                  = False


isomorphicPlus :: Tree -> Tree -> (Int -> Int -> Bool) -> Bool
isomorphic Leaf Leaf r                              = True
isomorphic (Tree x1 t11 t21) (Tree x2 t21 t22) r    = (isomorphic t11 t21) && (isomorphic t12 t22) && (r x1 x2)
isomorphic _ _ _                                    = False

sorted :: Tree -> Bool
sorted Leaf                         = True
sorted (Tree x Leaf Leaf)           = True
sorted (Tree x Leaf t)              = (x < (mintree t)) && (sorted t)
sorted (Tree x t Leaf)              = (x > (maxtree t)) && (sorted t)
sorted (Tree x t1 t2)               = (x > (maxtree t1)) && (sorted t1) && (x < (mintreeS t2)) && (sorted t2)

mintree :: Tree -> Int
mintree Leaf    = 0
minTree (Tree x t1 t2) 

addListToTree :: [Int] -> Tree -> Tree
addListToTree [] t      = t
addListToTree (x:xs) t  = addListToTree xs (insert x t)

insert :: Int -> Tree -> Tree
insert x Leaf   = (Node x Leaf Leaf)
insert x (Node y t1 t2)     | x == y    = Node y t1 t2  
                            | x > y     = Node y t1 (insert x t2)
                            | otherwise = Node y (insert x t1) t2















