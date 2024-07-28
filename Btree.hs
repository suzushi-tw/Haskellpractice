

data Bintree a = Empty | Node a (Bintree a) (Bintree a)
    deriving (Show, Eq)

leaf :: a -> Bintree a 
leaf x = Node x Empty Empty

insert :: Ord a => a-> Bintree a -> Bintree a 
insert x Empty = leaf x 
insert x (Node y left right)
 | x<y = Node y (insert x left) right
 | x>y = Node y left (insert x right)
 | otherwise = Node y left right 

-- check if the number is in binary tree 
elem' :: Ord a => a -> Bintree a -> Bool 
elem' _ Empty = False
elem' x (Node y left right)     
    | x<y = elem' x left 
    | x>y = elem' x right 
    | otherwise = True 


createtree :: Ord a => [a] -> Bintree a -> Bintree a 
createtree [] tree = tree 
createtree (x:xs) tree = createtree xs (insert x tree)

listtotree :: Ord a => [a] -> Bintree a 
listtotree xs = createtree xs Empty

main = do 
    let list = [6,8,12,1,3,9,15]
    let newtree = listtotree list 
    
    putStrLn "List created"