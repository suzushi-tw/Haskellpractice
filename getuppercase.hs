getuppercase str =[a | a <- str, a `elem` ['A'..'Z']]

len' xs= sum [ 1 | _ <-xs ]

pingpong xs=[ if x>10 then "ping" else "pong" | x <-xs, odd x]

main :: IO()
main=do 
    putStrLn "Get string uppercase "
    print(getuppercase "Hallo World")

    putStrLn "Get lengh of array "
    print(len' [2..8])

    putStrLn "pingpong from 5 to 15 "
    print(pingpong[5..15])
    putStrLn "Ping pong from 15 to 5"
    print(pingpong[15, 14..5])

    print([x*y | x<-[1,2,3], y<-[10,20,30]])
    print([x*y*z | x<-[1,2,3], y<-[10,20,30], z<-[4,5,6]])

    let adj=["brave", "modest", "gentle"]
    let ns=["tom", "jack", "ben"]
    print([a++ " " ++n | a<-adj, n<-ns ])
    print([a++" "++ show random | a<-adj, random<-[1 .. 10]])