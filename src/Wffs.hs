module Wffs (rgpli) where

import System.Random
import Data.List
import GPLIparser

-- here's the grammar we want to build from
-- wff  ::= "(" wff conn wff ")" | "~" wff | basic
-- conn ::= "<->", "->", "&", "v"
-- basic ::= "A"  | "B" | ... | "Z"

---randomchoice function

randomchoice g xs = getvalue randominteger $ enumerate xs
    where randominteger = fst (randomR (1, length xs) g)

randomchoices g x xs = getvalues randominteger $ enumerate xs
    where randominteger = take x (randomRs(1,(length xs)) g)

enumerate :: [a] -> [(Int,a)]
enumerate xs = zip [1..(length xs)] xs

getvalue :: Int -> [(Int,a)] -> a
getvalue x (y:ys) = if fst y == x
                    then snd y
                    else getvalue x ys

getvalues xs ys = [y | (z,y) <- ys, z `elem` xs]

-- these two are the main random functions we use. the first returns a random item from a list, the other returns n random items from a list

getrandom :: [a] -> IO a
getrandom xs = do
               newStdGen
               gen <- getStdGen
               let out = randomchoice gen xs
               return out 

getrandoms :: Int -> [a] -> IO [a]
getrandoms x xs = do
                  newStdGen
                  gen <- getStdGen
                  let out = randomchoices gen x xs
                  return out 

--symbols

predicates :: [String]
predicates = map (\x -> [x]) ['P','Q','R'] 

names :: [String]
names = map (\x -> [x]) ['a'..'d']

variables :: [String]
variables = map (\x -> [x]) ['x','y','z']

terms :: [String]
terms = names ++ variables

--atomics
--ideally, we'd like to enumerate just a few atomic formulas. 

getatomics :: IO [String]
getatomics = do
             nms <- getrandoms 3 names 
             vrs <- getrandoms 3 variables
             prs <- getrandoms 3 predicates 
             one <- getrandoms 3 (allatoms prs nms vrs)
             two <- getrandoms 3 (allatoms' prs nms vrs)
             thr <- getrandoms 3 (allatoms'' prs nms vrs)
             return (one ++ two ++ thr ++ (map ("~" ++) one) ++ (map ("~" ++) two) ++ (map ("~" ++) thr))

allatoms xs ys zs = [ x ++ w | x <- xs, w <- (ys ++ zs)]
allatoms' xs ys zs = [ x ++ w ++ u | x <- xs, w <- (ys ++ zs), u <- (ys ++ zs)]
allatoms'' xs ys zs = [ x ++ w ++ u ++ v | x <- xs, w <- (ys ++ zs), u <- (ys ++ zs), v <- (ys ++ zs)]

getidentities = getrandom [x ++ "=" ++ y | x <- terms, y <-terms]


---connectives

getconn :: IO String
getconn = do
          newStdGen
          gen <- getStdGen
          let out = randomchoice gen ["<->","->","&","v"]  
          return out

getneg :: IO String
getneg = do
         wff <- getwff 
         return ("~" ++ wff)

---quantifiers

quant :: IO String
quant = getrandom [ x ++ y | x <- ["@","#"], y <- variables]

getquant :: IO String
getquant = do
           wff <- getwff
           quant <- quant
           return (quant ++ wff)

---generator functions

getwffform x = getrandom ([getwff] ++ (concat (replicate 10 [getrandom x])) ++ [getneg] ++ [getquant] ++ [getidentities])




getwff :: IO String
getwff = do
         atms <- getatomics
         t <- getwffform atms
         x <- t
         y <- getconn
         u <- getwffform atms
         z <- u
         return ("(" ++ x ++ y ++ z ++ ")")

getwff' :: IO String
getwff' = do
          q <- quant
          x <- getwff
          return (q ++ x)

getwff'' :: IO String
getwff'' = do
           x <- getwff
           return ("~" ++ x)

getwff''' :: IO String
getwff''' = do
            q <- quant
            r <- quant
            x <- getwff
            return (q ++ r ++ x)

getwff'''' :: IO String
getwff'''' = do
             q <- quant
             r <- quant
             s <- quant
             x <- getwff
             return (q ++ r ++ s ++ x)

getwff''''' :: IO String
getwff''''' = do
              q <- getatomics
              r <- getrandom q
              return (r)

getwff'''''' :: IO String
getwff'''''' = do
               x <- quant
               q <- getatomics
               r <- getrandom q
               return (x ++ r)



getmain = getrandom [getwff, getwff',getwff', getwff'', getwff''', getwff'''', getwff''''',getwff'''''']

rgpli :: IO String
rgpli = do 
        y <- getmain
        x <- y
        if (closed (head (parser x)))
            then do 
                 return x
            else do 
                 rgpli

