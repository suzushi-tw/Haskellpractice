import Data.List 


main :: IO ()
main = do 
    putStrLn "seperate character"
    print(intersperse '.' "Hello")

    putStrLn "Inset into pairs"
    print(intercalate [0,0] [[1,2],[3,4],[5,6]])

    print "Transpose matrix"
    print(transpose [[1,2,3],[4,5,6],[7,8,9]])
    -- sum each array
    print(map sum $ transpose [[1,2,3], [4,5,6], [7,8,9]])