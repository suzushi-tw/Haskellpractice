getuppercase str =[a | a <- str, a `elem` ['A'..'Z']]

len' xs= sum [ 1 | _ <-xs ]

pingpong xs=[ if x>10 then "ping" else "pong" | x <-xs, odd x]