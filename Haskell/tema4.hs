data Tree a = Empty | Node a [Tree a] deriving Show

tree1 :: Tree Int
tree1 = Node 1 [ Node 2 [ Node 4 []
                        , Node 5 []
                        , Node 6 []]
                ,Node 3 [ Node 7 []]]

sizeT :: Tree a -> Int
sizeT Empty = 0
sizeT (Node x ts) = 1 + sum (map sizeT ts) -- sum [ sizeT t | t <- ts] Por compresión

sumT :: (Num a) => Tree a -> a
sumT Empty = 0
sumT (Node x ts) = x + sum (map sumT ts)

heightT :: Tree a -> Int
heightT Empty = 0
heightT (Node x []) = 1
heightT (Node x ts) = 1+ maximum [heightT t | t <- ts ]

containsT :: (Eq a) => a -> Tree a -> Bool
containsT x Empty = False
containsT x (Node y ts) = x == y || or [ containsT x t | t <- ts]

atLevelT :: Int -> Tree a -> [a]
atLevelT n Empty = []
atLevelT n (Node x ts) 
        | n < 0 = error "Nivel Negativo"
        | n == 0 = [x]
        | otherwise = concat [atLevelT (n-1) t | t <- ts ]

leafsT :: Tree a -> [a]
leafsT Empty = []
leafsT (Node x []) = [x]
leafsT (Node x ts) = concat [ leafsT t | t <- ts ]

data TreeB a = EmptyB | NodeB a (TreeB a) (TreeB a) deriving Show

tree2 :: TreeB Int
tree2 = NodeB 1 ( NodeB 2 (NodeB 4 EmptyB EmptyB)
                          (NodeB 5 EmptyB EmptyB))
               ( NodeB 3 (Node 6 EmptyB EmptyB) EmptyB)
sizeB :: Tree a -> a
sizeB EmptyB = 0
sizeB (NodeB x lt rt) = 1 + sizeB lt + sizeB rt

heightB :: TreeB a -> Int
heightB EmptyB = 0
heightB (NodeB x lt rt) = 1 + max (heightB lt) (heightB rt)
