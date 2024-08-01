import Data.List
import System.IO
import Text.Read(readMaybe)
import Data.Char

readwords :: IO ()
readwords = do
    putStrLn "Deutsch"
    deutsch <- getLine 
    putStrLn "Bairisches"
    bairisches <- getLine 
    putStrLn (deutsch ++ "heisst auf Bairisch" ++ bairisches) 
    if deutsch /= ""
        then readwords
    else  
        putStrLn "Auf wiedersehen !"

createlibaray :: IO ()
createlibaray = readwords

wordstupel :: IO ()
wordstupel = do
    putStrLn "Deutsch"
    deutsch <- getLine 
    putStrLn "Bairisches"
    bairisches <- getLine 
    putStrLn (deutsch ++ "heisst auf Bairisch" ++ bairisches) 
    let tuplestring = "(" ++ deutsch ++ "," ++ bairisches ++ ")"
    putStrLn (tuplestring)
    appendFile "Tuple.txt" (tuplestring ++ "\n")
    if deutsch /= ""
        then wordstupel
    else  
        putStrLn "Auf wiedersehen !"

createtupel :: IO ()
createtupel = wordstupel 

dictlookup :: [(String, String)] -> String -> [String]
dictlookup dict word = [bairisch | (deutsch, bairisch)<-dict, deutsch == word]

lookupword :: IO()
lookupword = do 
    contents <- readFile "Tuple.txt" 
    let tuples = map read $ lines contents :: [(String, String)]

    putStrLn "Enter the word you are looking for"
    word <- getLine

    print ( dictlookup tuples word )


main = do 
    putStrLn "Uppercase or lowercase"
    choice <- getLine
    getfile <- openFile "IO.txt" ReadMode
    contents <- hGetContents getfile
    let result= if choice == "upper" 
                    then map toUpper contents
                else if choice == "lower" 
                    then map toLower contents
                else contents
    putStrLn result 
    
    hClose getfile 