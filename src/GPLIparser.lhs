> module GPLIparser (parser, Prop (..), Predicate(..), getpredicates1, getpredicates2, getpredicates3, getnames, rmdups, closed) where

> import Control.Monad (liftM, ap)
> import Data.Char
> import Data.List

-- parser function to export

> parser :: String -> [Prop]
> parser x = if null (apply prop x) ||not  (null (snd (head (apply prop x))))
>            then [] 
>            else [fst $ head $ apply prop x] 

-- general parser functions

> newtype Parser a = Parser (String -> [(a,String)])
> apply :: Parser a -> String -> [(a,String)]
> apply (Parser p) s = p s

> parse :: Parser a -> String -> a
> parse p = fst . head . apply p

--- type declaration

> instance Monad Parser where
>    return x = Parser (\s -> [(x,s)])
>    p >>= q  = Parser (\s -> [(y,s2)
>                            | (x,s') <- apply p s,
>                              (y,s2) <- apply (q x) s'])

> instance Functor Parser where
>    fmap = liftM

> instance Applicative Parser where
>     pure = return
>     (<*>) = ap

> getc :: Parser Char
> getc = Parser f
>        where f [] = []
>              f (c:cs) = [(c,cs)]

> sat :: (Char -> Bool) -> Parser Char
> sat p = do {c <- getc;
>            if p c then return c
>            else ffail}

> ffail = Parser (\s -> [])

> char :: Char -> Parser ()
> char x = do {c <- sat (==x); return ()}

> string :: String -> Parser ()
> string [] = return ()
> string (x:xs) = do {char x; string xs; return ()}

--- choice operator for parsers

> (<|>) :: Parser a -> Parser a -> Parser a
> p <|> q = Parser f
>           where f s = let ps = apply p s in 
>                       if null ps then apply q s
>                       else ps

-- parsers for GPLI

> data Prop = Atomic Predicate [Char]
>           | Negation Prop
>           | Conjunction Prop Prop
>           | Disjunction Prop Prop
>           | Conditional Prop Prop
>           | Biconditional Prop Prop
>           | Existential Char Prop
>           | Universal Char Prop
>           deriving (Show)

> data Predicate = Predicate1 Char
>                | Predicate2 Char
>                | Predicate3 Char
>                deriving (Show)


> atomic' :: Parser Prop
> atomic' = do x <- preds
>              y <- term
>              return (Atomic (Predicate1 x) [y])

> atomic2 :: Parser Prop
> atomic2 = do x <- preds
>              y <- term
>              z <- term
>              return (Atomic (Predicate2 x) [y,z])

> atomic3 :: Parser Prop
> atomic3 = do x <- preds
>              y <- term
>              z <- term
>              u <- term
>              return (Atomic (Predicate3 x) [y,z,u])

> preds :: Parser Char
> preds = do c <- sat (`elem` ['A'..'Z'])
>            return (c)


> variables :: [Char]
> variables = ['u','w','y','x','z'] 

> names :: [Char] 
> names = ['a'..'t']

> terms :: [Char]
> terms = variables ++ names

> term :: Parser Char
> term = do c <- sat (`elem` terms)
>           return (c)

> neg :: Parser Prop
> neg = do c <- sat (=='~')
>          x <- prop 
>          return (Negation x)

> conj :: Parser Prop
> conj = do x <- parens
>           y <- prop
>           z <- conj'
>           u <- prop
>           t <- parens
>           return (Conjunction y u)  

> disj :: Parser Prop
> disj = do x <- parens
>           y <- prop
>           z <- disj'
>           u <- prop
>           t <- parens
>           return (Disjunction y u)  

> cond :: Parser Prop
> cond = do x <- parens
>           y <- prop
>           z <- cond'
>           u <- prop
>           t <- parens
>           return (Conditional y u)  

> bicon :: Parser Prop
> bicon = do x <- parens
>            y <- prop
>            z <- bicon'
>            u <- prop
>            t <- parens
>            return (Biconditional y u)  

> parens :: Parser ()
> parens = do c <- sat (=='(') 
>             return () 
>         <|> do c <- sat (==')')
>                return ()

> conj' :: Parser ()
> conj' = do c <- string' "&"
>            return ()

> disj' :: Parser () 
> disj' = do c <- string'  "v"
>            return ()

> cond' :: Parser ()
> cond' = do c <- string' "->" 
>            return ()

> bicon' :: Parser ()
> bicon' = do c <- string' "<->"
>             return ()

> string' :: String -> Parser String
> string' [] = return []
> string' (x:xs) = do char x
>                     string' xs
>                     return (x:xs)

> exi :: Parser Prop
> exi = do x <- sat (== '#') 
>          c <- sat (`elem` variables)
>          p <- prop
>          return (Existential c p)
 
> uni :: Parser Prop
> uni = do x <- sat (== '@') 
>          c <- sat (`elem` variables)
>          p <- prop
>          return (Universal c p)

> iden :: Parser Prop
> iden = do x <- sat (`elem` terms)
>           y <- sat (=='=')
>           z <- sat (`elem` terms)
>           return (Atomic (Predicate2 'I') [x,z])

> niden :: Parser Prop
> niden = do x <- sat (`elem` terms)
>            y <- string' "/="
>            z <- sat (`elem` terms)
>            return (Negation (Atomic (Predicate2 'I') [x,z]))

> prop :: Parser Prop
> prop = do x <- atomic3 
>           return (x)
>        <|> do x <- atomic2
>               return (x)
>        <|> do x <- atomic'
>               return (x)
>        <|> do x <- niden
>               return (x)
>        <|> do x <- iden 
>               return (x)
>        <|> do x <- conj
>               return (x)
>        <|> do x <- disj
>               return (x)         
>        <|> do x <- bicon
>               return (x)
>        <|> do x <- cond
>               return (x)
>        <|> do x <- uni
>               return (x)
>        <|> do x <- exi
>               return (x)
>        <|> do x <- neg
>               return (x)


END PARSER

Some helper functions.

> getpredicates1 :: Prop -> [Char]
> getpredicates1 (Negation x) = nub $ getpredicates1 x
> getpredicates1 (Disjunction x y) = nub $ (getpredicates1 x) ++ (getpredicates1 y)
> getpredicates1 (Conjunction x y) = nub $ (getpredicates1 x) ++ (getpredicates1 y)
> getpredicates1 (Conditional x y) = nub $ (getpredicates1 x) ++ (getpredicates1 y)
> getpredicates1 (Biconditional x y) = nub $ (getpredicates1 x) ++ (getpredicates1 y)
> getpredicates1 (Existential x y) = nub $ (getpredicates1 y)
> getpredicates1 (Universal x y) = nub $ (getpredicates1 y)
> getpredicates1 (Atomic (Predicate1 x) y) = [x]
> getpredicates1 (Atomic (Predicate2 x) y) = []
> getpredicates1 (Atomic (Predicate3 x) y) = []


> getpredicates2 :: Prop -> [Char]
> getpredicates2 (Negation x) = nub $ getpredicates2 x
> getpredicates2 (Disjunction x y) = nub $ (getpredicates2 x) ++ (getpredicates2 y)
> getpredicates2 (Conjunction x y) = nub $ (getpredicates2 x) ++ (getpredicates2 y)
> getpredicates2 (Conditional x y) = nub $ (getpredicates2 x) ++ (getpredicates2 y)
> getpredicates2 (Biconditional x y) = nub $ (getpredicates2 x) ++ (getpredicates2 y)
> getpredicates2 (Existential x y) = nub $ (getpredicates2 y)
> getpredicates2 (Universal x y) = nub $ (getpredicates2 y)
> getpredicates2 (Atomic (Predicate2 x) y) = [x]
> getpredicates2 (Atomic (Predicate1 x) y) = []
> getpredicates2 (Atomic (Predicate3 x) y) = []

> getpredicates3 :: Prop -> [Char]
> getpredicates3 (Negation x) = nub $ getpredicates3 x
> getpredicates3 (Disjunction x y) = nub $ (getpredicates3 x) ++ (getpredicates3 y)
> getpredicates3 (Conjunction x y) = nub $ (getpredicates3 x) ++ (getpredicates3 y)
> getpredicates3 (Conditional x y) = nub $ (getpredicates3 x) ++ (getpredicates3 y)
> getpredicates3 (Biconditional x y) = nub $ (getpredicates3 x) ++ (getpredicates3 y)
> getpredicates3 (Existential x y) = nub $ (getpredicates3 y)
> getpredicates3 (Universal x y) = nub $(getpredicates3 y)
> getpredicates3 (Atomic (Predicate3 x) y) = [x]
> getpredicates3 (Atomic (Predicate1 x) y) = []
> getpredicates3 (Atomic (Predicate2 x) y) = []


> getnames :: [Char] -> [Char]
> getnames xs = nub [ x | x <- xs, x `elem` names]

> rmdups :: Ord a => [a] -> [a]
> rmdups = map head . group . sort

CHECK FOR OPEN FORMULA
======================

 data Prop = Atomic Predicate [Char]
           | Negation Prop
           | Conjunction Prop Prop
           | Disjunction Prop Prop
           | Conditional Prop Prop
           | Biconditional Prop Prop
           | Existential Char Prop
           | Universal Char Prop
           deriving (Show)

 data Predicate = Predicate1 Char
                | Predicate2 Char
                | Predicate3 Char
                deriving (Show)


First we write a function for replacing variables with *. 

> rplce :: Char -> Prop -> Prop
> rplce v (Atomic z ys) = (Atomic z (ast v ys)) 
> rplce v (Negation x) = (Negation (rplce v x))  
> rplce v (Conjunction l r) = (Conjunction (rplce v l) (rplce v r))
> rplce v (Disjunction l r) = (Disjunction (rplce v l) (rplce v r))
> rplce v (Conditional l r) = (Conditional (rplce v l) (rplce v r))
> rplce v (Biconditional l r) = (Biconditional (rplce v l) (rplce v r))
> rplce v (Existential x p) = (Existential (ast' v x) (rplce v p))
> rplce v (Universal x p) = (Universal (ast' v x) (rplce v p))

> ast v xs = map (ast' v) xs

> ast' v x = if x == v
>            then '*'
>            else x  

Second we write a function which sends rplc down the line when it encounters an existential or a universal with x as the variable.

> rp (Existential x p) = rplce x (Existential x p) 
> rp (Universal x p) = rplce x (Universal x p) 
> rp (Atomic x ys) = (Atomic x ys)
> rp (Conjunction l r) = (Conjunction (rp l) (rp r))
> rp (Disjunction l r) = (Disjunction (rp l) (rp r))
> rp (Conditional l r) = (Conditional (rp l) (rp r))
> rp (Biconditional l r) = (Biconditional (rp l) (rp r))
> rp (Negation x) = (Negation (rp x))

Third, we write a function to check if there are any variables left in the formula.  

> ch (Existential x p) = (ch p)
> ch (Universal x p) = (ch p) 
> ch (Atomic x ys) = ys
> ch (Conjunction l r) =  (ch l) ++ (ch r)
> ch (Disjunction l r) =  (ch l) ++ (ch r)
> ch (Conditional l r) = (ch l) ++ (ch r)
> ch (Biconditional l r) = (ch l) ++ (ch r)
> ch (Negation x) = (ch x)

> check xs = [ x | x <- xs, x `elem` variables]

> closed p = if null (check (ch (rp p)))    
>            then True
>            else False 
