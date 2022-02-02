enumFrom2 :: Int -> Int -> [Int]
enumFrom2 n m | n<m = n : enumFrom2 (n+1) m
              | n>m = n : enumFrom2 (n-1) m
              | otherwise = [m]

enumFromThen2 :: Int -> Int -> Int -> [Int]
enumFromThen2 m s n | m <= s && s <= n = m : enumFromThen2 s (2*s-m) n
                    | m >= s && s >= n = m : enumFromThen2 s (2*s-m) n
                    | otherwise = [m]

plus1 :: [a] -> [a] -> [a]
plus1 [] l1 = l1
plus1 l1 [] = l1
plus1 (a:b) (h:t) = a : plus1  b (h:t)

exclamation2 :: [a] -> Int -> a
exclamation2 (h:t) 0 = h
exclamation2 (h:t) n = exclamation2 t (n-1)

exclamation :: [a] -> Int -> a
exclamation (h:t) x
     | x == 0 = h
     | otherwise = exclamation t (x-1)

reverse2 :: [a] -> [a]
reverse2 [] = []
reverse2 (h:t) = last(h:t) : reverse2 (init (h:t))

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (h:t) = reverse' t ++ [h]

take' :: Int -> [a] -> [a]
take' _ [] = []
take' 0 _ = []
take' x (h:t) = h : take' (x-1) t

drop2 :: Int -> [a] -> [a]
drop2 _ [] = []
drop2 0 l1 = l1
drop2 n (h:t) = drop2 (n-1) t

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (h:t) (a:b) = (h,a) : zip' t b  

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' x y = y : replicate' (x-1) y

intersperse' :: a -> [a] -> [a] 
intersperse' x [h] = [h]
intersperse' x (h:t) = h:x : intersperse' x t 

group' :: Eq a => [a] -> [[a]]
group' [] = []

group' (h:m:t)  | h == m = [h] : group'(m:t)
                | otherwise = [h] : group' (m:t)


group2 :: Eq a => [a] -> [[a]]
group2 [] = []
group2 (h:m:t) = aux [h] h m t
      where aux n h m [] = if h == m then [n ++ [m]] else n : [[m]]
            aux n h m t | h == m = aux (n ++ [h]) m (head t) (tail t)
                        | otherwise = n : aux [m] m (head t) (tail t)

concat2 :: [[a]] -> [a]
concat2 [] = []
concat2 ([]:z) = concat2 z
concat2 ((h:t):z) = h : concat2 ((t):z)

concat' :: [[a]] -> [a]
concat' [] = []
concat' (h:t) = h ++ concat' t

inits' :: [a] -> [[a]]
inits' (h:t) = go [] (h:t)
      where
          go a [] = a : []
          go a (h:t) = a : go (a ++ [h]) t 

tails2 :: [a] -> [[a]]
tails2 [] = [[]]
tails2 (h:t) = (h:t) : tails2 t

heads2 :: [[a]] -> [a]
heads2 [] = []
heads2 ([]:z) = heads2 z
heads2 ((h:t):z) = h : heads2 z

total2 :: [[a]] -> Int
total2 [] = 0
total2 ((h:t):z) = go 0 ((h:t):z)
             where go n []        = n
                   go n ([]:z)    = go n z
                   go n ([h]:z)   = go (n+1) z  -- but whay
                   go n ((h:t):z) = go (n+1) ((t):z)

total1 :: [[a]] -> Int
total1 []        = 0 
total1 ([]:z)    = total1 z
total1 ((h:t):z) = 1 + total1 ((t):z)

fun2 :: [(a,b,c)] -> [(a,c)]
fun2 [] = []
fun2 ((a,b,c):z) = (a,c) : fun2 z

cola2 :: [(String,b,c)] -> String
cola2 [] = []
cola2 ((a,b,c):z) = a ++ cola2 z

idade' :: Int -> Int -> [(String,Int)] -> [String]
idade' n m [] = []
idade' n m ((a,b):t) | n-m >= b  = a : idade' n m t
                     | otherwise = idade' n m t
                     
powerEnumFrom' :: Int -> Int -> [Int]
powerEnumFrom' n m = go n m 0

               where go n m p | p < m = n^p : go n m (p+1)
                              | otherwise = []

isPrime2 :: Int -> Bool
isPrime2 1 = False
isPrime2 n = go n 2
         where go n m | m^2 < n && mod n m /= 0 = go n (m+1)
                      | m^2 < n && mod n m == 0 = False
                      | m^2 >= n = True

isPrime' :: Int -> Bool
isPrime' 1 = False
isPrime' n = go n 2  
         where go n m | n >= m^2 && mod n m /= 0 = go n (m+1)
                      | n >= m^2 && mod n m == 0 = False
                      | otherwise = True

isPrefixOf' :: Eq a => [a] -> [a] -> Bool
isPrefixOf' [] _ = True
isPrefixOf' _ [] = False
isPrefixOf' (h:t) (a:b) | a == h = isPrefixOf' t b
                        | otherwise = False

isSuffixOf' :: Eq a => [a] -> [a] -> Bool
isSuffixOf' [] _ = True
isSuffixOf' _ [] = False
isSuffixOf' (h:t) (a:b) | last (h:t) == last (a:b) = isSuffixOf' (init(h:t)) (init (a:b))
                        | otherwise = False

isSubsequenceOf' :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf' [] _ = True
isSubsequenceOf' _ [] = False
isSubsequenceOf' (h:t) (a:b) | h /= a = isSubsequenceOf' (h:t) b
                             | otherwise = isSubsequenceOf' t b

elemIndices2 :: Eq a => a -> [a] -> [Int]
elemIndices2 n (h:t) = go n 0 (h:t)
                     where go n m [] = []
                           go n m (h:t) | n == h = m : go n (m+1) t
                                        | otherwise = go n (m+1) t
                                      
nub' :: Eq a => [a] -> [a]
nub' [] = []
nub' (h:t) | elem' h t = nub' t
           | otherwise = h : nub' t
                     where elem' h [] = False
                           elem' h (c:t) | h /= c = elem' h t
                                         | otherwise = True
                         
delete' :: Eq a => a -> [a] -> [a]
delete' n [] = []
delete' n (h:t) | n /= h = h: delete' n t
                | otherwise = t

bb :: Eq a => [a] -> [a] -> [a]
bb l1 [] = l1
bb [] l1 = []
bb (h:t) (a:b) = bb (delete2 a (h:t)) b

                 where delete2 n [] = []
                       delete2 n (h:t) | n /= h = h : delete2 n t
                                       | otherwise = t

union2 :: Eq a => [a] -> [a] -> [a]
union2 l1 [] = l1
union2 [] l1 = l1
union2 l1 (h:t) | elem3 h l1 = union2 l1 t
                | otherwise = union2 (l1 ++ [h]) t

                   where elem3 a [] = False
                         elem3 a (c:t) | a /= c = elem3 a t
                                       | otherwise = True


union' :: Eq a => [a] -> [a] -> [a]
union' l1 [] = l1
union' [] l1 = []
union' (h:t) (a:b) | elem2 a (h:t) = union' (h:t) b
                   | otherwise =  (h:t) ++ [a]

                      where elem2 h [] = False
                            elem2 h (c:t) | h /= c = elem2 h t
                                          | otherwise = True
                         
intersect' :: Eq a => [a] -> [a] -> [a]
intersect' [] _ = []
intersect' l1 [] = l1
intersect' (h:t) (a:b) | elem3 h (a:b) = h : intersect' t (a:b)
                       | otherwise = intersect' t (a:b)
                          
                       where elem3 n [] = False
                             elem3 n (h:t) | n /= h = elem3 n t
                                              | otherwise = True

insert2 :: Ord a => a -> [a] -> [a]
insert2 n []    = [n]
insert2 n (h:t) | n > h = h : insert2 n t
                | otherwise = n : (h:t)

insert' :: Ord a => a -> [a] -> [a]
insert' n [] = [n]
insert' n (h:t) | n < h = n:(h:t)
                | otherwise = h : insert' n t

unwords' :: [String] -> String
unwords' [] = []
unwords' (h:t) = h ++ " " ++ unwords' t

unlines' :: [String] -> String
unlines' [] = []
unlines' (h:t) = h ++ "\n" ++ unlines' t

pMaior' :: Ord a => [a] -> Int
pMaior' [] = error "Lista Vazia"
pMaior' (h:m:t) = go 0 (h:m:t)

             where 
                   go n (h:t) | maior h t = n
                              | otherwise = go (n+1) t

                                       where maior a [] = True
                                             maior a (h:t) | a >= h = maior a t
                                                           | otherwise = False

lookup2 :: Eq a => a -> [(a,b)] -> Maybe b
lookup2 n [] = Nothing
lookup2 n ((a,b):c) | n == a = Just b
                    | otherwise = lookup2 n c

preCrescente' :: Ord a => [a] -> [a]
preCrescente' [] = []
preCrescente' (h:m:t) | h < m = h: preCrescente' (m:t)
                      |otherwise = [h]

iSort' :: Ord a => [a] -> [a]
iSort' [] = []
iSort' (h:t) = insert h (iSort' t)
                 where insert n [] = [n]
                       insert n (h:t) | n < h = n:(h:t)
                                      | otherwise = h : insert n t

menor' :: String -> String -> Bool
menor' _ [] = False
menor' [] _ = True 
menor'(h:t) (a:b) | h == a = menor' t b 
                  | h > a  = False
                  | h < a  = True

elemMSet' :: Eq a => a -> [(a,Int)] -> Bool
elemMSet' n [] = False
elemMSet' n ((a,b):c) | n /= a = elemMSet' n c
                      | otherwise = True

converteMSet' :: [(a,Int)] -> [a]
converteMSet' [] = []
converteMSet' ((a,0):c) = converteMSet' c
converteMSet' ((a,b):c) = a : converteMSet'((a, b-1):c)    

insereMSet' :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet' n [] = [(n,1)]
insereMSet' n ((a,b):c) | n == a = (a, b+1):c
                       | otherwise = (a,b) : insereMSet' n c

removeMSet' :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet' n [] = []
removeMSet' n ((a,1):c) | n == a = removeMSet' n c
                        | otherwise = (a,1) : removeMSet' n c
removeMSet' n ((a,b):c) | n == a = (a, b-1):c
                        | otherwise = (a,b) : removeMSet' n c

catMaybes2 :: [Maybe a] -> [a]
catMaybes2 [] = []
catMaybes2 (h:t) = case h of
        Nothing -> catMaybes2 t
        Just h -> h: catMaybes2 t

data Movimento = Norte | Sul | Este | Oeste
                deriving Show

caminho' :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho' (x1,y1) (x2,y2) | x1 > x2    = Oeste : caminho' (x1-1,x2) (y1,y2)
                         | x1 < x2    = Este  : caminho' (x1,x2-1) (y1,y2)
                         | y1 > y2    = Sul   : caminho' (x1,x2) (y1-1,y2)
                         | y1 < y2    = Norte : caminho' (x1,x2) (y1,y2-1)
                         | otherwise  = []

type Ponto = (Float,Float)
data Rectangulo = Rect Ponto Ponto

contaQuadrados :: [Rectangulo] -> Int
contaQuadrados [] = 0
contaQuadrados ((Rect (x1, y1) (x2, y2)):t) | abs(y1-y2) == abs (x1-x2) = 1 + contaQuadrados t

areaTotal :: [Rectangulo] -> Float
areaTotal [] = 0
areaTotal ((Rect (x1, y1) (x2, y2)):t) = abs(y1-y2) * abs (x1-x2) + areaTotal t


data Equipamento = Bom | Razoavel | Avariado
                 deriving Show

naoReparar :: [Equipamento] -> Int
naoReparar []    = 0
naoReparar (h:t) = 
      case h of
            Avariado -> 1 + naoReparar t
            Bom      -> naoReparar t
            Razoavel -> naoReparar t
            

