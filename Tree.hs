data BinaryTree a = Empty
                  | Node a (BinaryTree a) (BinaryTree a)
                  deriving (Show, Eq)

-- Insert a value into the tree
insert :: (Ord a) => a -> BinaryTree a -> BinaryTree a
insert x Empty = Node x Empty Empty
insert x (Node a left right)
    | x == a = Node x left right  -- Value already exists
    | x < a  = Node a (insert x left) right
    | x > a  = Node a left (insert x right)

-- Check if a value exists in the tree
contains :: (Ord a) => a -> BinaryTree a -> Bool
contains _ Empty = False
contains x (Node a left right)
    | x == a = True
    | x < a  = contains x left
    | x > a  = contains x right

-- Inorder traversal of the tree
inorder :: BinaryTree a -> [a]
inorder Empty = []
inorder (Node a left right) = inorder left ++ [a] ++ inorder right

-- Main function to demonstrate the binary tree
main :: IO ()
main = do
    let values = [5, 3, 7, 1, 9, 4, 6, 8]
    let tree = foldr insert Empty values
    
    putStrLn "Binary Tree created with values:"
    print values
    
    putStrLn "\nInorder traversal of the tree:"
    print (inorder tree)
    
    putStrLn "\nChecking if values exist in the tree:"
    mapM_ (\x -> putStrLn $ show x ++ " exists: " ++ show (contains x tree)) [1..10]