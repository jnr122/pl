


--string builder
--["Hello", "World"] ", " -> "Hello, World"
stringBuild :: [String] -> String -> String
stringBuild xs s = case xs of
  [] -> ""
  x:xs' -> x ++ s ++ (stringBuild xs' s)

--def dataTypes
data CoolBool = CoolTrue
          | CoolFalse

--multiple cons/ pattern matching
--THIS
startsWithHiUgly :: String -> Bool
startsWithHiUgly cs =
  case cs of
    [] -> False
    c:cs' -> if c == 'H'
             then case cs' of
                    [] -> False
                    c'':cs'' -> if c'' == 'i'
                                then True
                                else False
             else False
                  
--CAN be written as this
startsWithHi :: String -> Bool
startsWithHi cs = case cs of
  'H':'i': _ -> True
  _ -> False

--generalized for any two chars
startsWithTwoChars :: Char -> Char -> String -> Bool
startsWithTwoChars c1 c2 cs = case cs of
  a:b: _ -> (a == c1) && (b == c2) --avoid binding by changing var names
  _ -> False

--mapping
mapEx :: [Int] -> [String]
mapEx = map (\x -> (show x) ++ " is a number")  --can exclude arg type

--easy pig latin
--["hello", "there"] -> ["elloH", "hereT"]
pigLatin :: [String] -> [String]
pigLatin xs = case xs of
  [] -> []
  x:xs' -> ((tail x) ++ ([head x])) : (pigLatin xs')

--append
append :: [Int] -> [Int] -> [Int]
append xs ys = case xs of
  [] -> ys
  x:xs' -> append (init xs) (last xs:ys)

--reverse string
reverse' :: String -> String
reverse' xs = case xs of
  [] -> []
  x:xs' -> (reverse' xs') ++ [x]

--palindrome w/ reverse
palindrome :: String -> Bool
palindrome xs = xs == (reverse xs)

--palindrome
palindrome' :: String -> Bool
palindrome' xs = case xs of
  [] -> True
  x:[] -> True
  x:xs' -> x == (last xs') && (palindrome' (init xs'))

--fib
fib :: Int -> Int
fib n = case n of
  1 -> n
  0 -> n
  _ -> fib (n-2) + fib (n-1) 
  
