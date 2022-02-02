import Data.Char
import Data.List
--1
perimetro :: Float -> Float
perimetro r = 2*pi*r

--2
dist :: (Double, Double) -> (Double, Double) -> (Double, Double)
dist (n,m) (p,q) = (abs(n-p),abs(m-q))

--3
primUlt :: [Float] -> (Float, Float)
primUlt (h:tail) = (h, last tail)

--4
multiplo :: Int -> Int -> Bool
multiplo m n | mod m n == 0 = True
             | otherwise = False

--5
truncaImpar :: [Float] -> [Float]
truncaImpar (h:t) | mod (length (h:t)) 2 == 0 = h:t
              | otherwise = t

--6
max2 :: Int -> Int -> Int
max2 n m | n > m = n
         | otherwise = m

--7 
max3 :: Int -> Int -> Int -> Int
max3 m n p | max2 m n > max2 n p = max2 m n
           | otherwise = max2 n p

--8
nRaizes :: Float -> Float -> Float -> Float
nRaizes a b c | aux a b c > 0 = 2
              | aux a b c < 0 = 0
              | otherwise = 1
        where 
            aux a b c = b^2 - 4*a*c

--9
raizes :: Float -> Float -> Float -> [Float]
raizes a b c | nRaizes a b c == 2 = [((-b + b^2 - 4*a*c)/2*a), ((-b - b^2 - 4*a*c)/2*a)]
             | nRaizes a b c == 0 = []
             | nRaizes a b c == 1 = [(-b /2*a)]


type Hora = (Int, Int)

--10
certo :: (Int,Int) -> Bool
certo (a,b) | 0 <= a && a < 24 && 0 <= b && b < 60 = True
            | otherwise = False

--11
depois :: (Int, Int) -> (Int, Int) -> Bool
depois (a,b) (c,d) | a>c = True
                   | a<c = False
                   | a == c && b > d = True
                   | otherwise = False
--12
emMinutos :: (Hora) -> Int
emMinutos (a,b) = a*60 + b

--13
emHoras :: Int -> (Hora)
emHoras a | div a 60 < 24 = (div a 60, mod a 60)
          | otherwise = (mod (div a 60) 24, mod a 60)

--14
diferenca :: (Hora) -> (Hora) -> Int
diferenca (a,b) (c,d) = abs ( emMinutos (a,b) - emMinutos (c,d))

--15
add :: (Int, Int) -> Int -> (Int,Int)
add (a,b) c = emHoras ( emMinutos (a,b) + c)

data Semaforo = Verde | Amarelo | Vermelho deriving (Show, Eq)
--16

next :: Semaforo -> Semaforo
next a | a == Vermelho = Verde
       | a == Verde = Amarelo
       | a == Amarelo = Vermelho

--17 

stop :: Semaforo -> Bool
stop a | a == Vermelho = True
       | otherwise = False

--18
safe :: Semaforo -> Semaforo -> Bool
safe a b | a == Vermelho || b == Vermelho = True
         | otherwise = False

--19
data Ponto = Cartesiano Double Double | Polar Double Double deriving (Show, Eq)

posx :: Ponto -> Double
posx a = case a of 
    Cartesiano b c -> b
    Polar b c -> cos c * b

--20
posy :: Ponto -> Double
posy a = case a of
    Cartesiano b c -> c
    Polar b c -> sin c * b

--21
raio :: Ponto -> Double
raio a = case a of
    Cartesiano b c -> sqrt (b^2 + c^2)
    Polar b c -> b

--22
angulo :: Ponto -> Double
angulo a = case a of
    Cartesiano b c -> atan(c/b)
    Polar b c -> c

--23


--24
data Figura = Circulo Ponto Double
            | Rectangulo Ponto Ponto
            | Triangulo Ponto Ponto Ponto
            deriving (Show, Eq)

poligono :: Figura -> Bool
poligono a = case a of
      Triangulo b c d -> True
      Rectangulo b c -> True
      Circulo b c -> False

--25


--26
isLower :: Char -> Bool 
isLower a | ord a >= 97 && ord a <= 122 = True
          | otherwise = False

--27
isDigit :: Char -> Bool 
isDigit a | ord a > 47 && ord a < 58 = True
          | otherwise = False

--28
intToDigit :: Int -> Char
intToDigit a = chr (a + 48)

--29
digitToInt :: Char -> Int
digitToInt a = ord a - 48

--30 Ficha 3
data Horas = H Int Int deriving Show
type Etapa = (Horas, Horas)
type Viagem = [Etapa]

welltapa :: Etapa -> Bool
welltapa (H b c, H e f) | b < e = True
                    | b == e && c < f = True
                    | otherwise = False

--31
viagemwells :: Viagem -> Bool
viagemwells [(H a b, H c d)] = True
viagemwells ((H a b, H c d):(H e f, H g h):t) | c < e = viagemwells ((H e f, H g h):t) 
                                              | c == e && d < h = viagemwells ((H e f, H g h):t)
                                              | otherwise = False

--32
calcular :: Viagem -> Etapa
calcular ((H a b, H c d):t) = (H a b, snd(last t)) 

--33a
toMinutos :: Horas -> Int
toMinutos (H a b) = a * 60 + b


--33b
toHoras :: Int -> Horas
toHoras a | div a 60 < 24 = H (div a 60) (mod a 60)
          | otherwise = H (mod (div a 60) 24) (mod a 60)
                          
--33
tempoT :: Viagem -> Int
tempoT ((H a b, H c d):t) | viagemwells ((H a b, H c d):t) = toMinutos (H c d) - toMinutos (H a b) + tempoT t 
                          | otherwise = 0

--34
tempoE :: Viagem -> Int
tempoE ((H a b, H c d):(H e f, H g h):t) | viagemwells ((H a b, H c d):t) = (toMinutos (H e f) - toMinutos (H c d)) + tempoE ((H e f, H g h):t)
                                         | otherwise = 0
      
--35 
tempoTo :: Viagem -> Int
tempoTo ((H a b, H c d):(H e f, H g h):t) = tempoT ((H a b, H c d):t) + tempoE ((H a b, H c d):(H e f, H g h):t)

--35b
tempoTo2 :: Viagem -> Int
tempoTo2 ((H a b, H c d):t) = toMinutos (snd(last t)) - toMinutos (H a b)

--36
data Contacto  = Casa Integer
               | Trab Integer
               | Tlm Integer
               | Email String
               deriving Show
type Nome = String
type Agenda = [(Nome, [Contacto])]

acrescEmail :: Nome -> String -> Agenda -> Agenda
acrescEmail nome email []            = [(nome, [Email email])]
acrescEmail nome email ((n, cont):t) | nome == n = (n, aux email cont) :t
                                     | otherwise = (n, cont) : acrescEmail nome email t

                    where
                        aux email [] = [Email email]
                        aux email (h:t) = case h of 
                            Email a -> if a == em99ail then (h:t) else h : aux email t
                            _ -> aux email t


--37
verEmails :: Nome -> Agenda -> Maybe [String]
verEmails nome [] = Nothing
verEmails nome ((n, cont):t) | nome == n = Just (aux cont)
                             | otherwise = verEmails nome t
            where
                aux [] = []
                aux (h:t) = case h of
                    Email a -> a : aux t 
                    _ -> aux t

--38
consTelefs :: [Contacto] -> [Integer]
consTelefs [] = []
consTelefs (h:t) = case h of
    Casa a -> a : consTelefs t
    Tlm a -> a : consTelefs t
    Trab a -> a : consTelefs t
    _ -> consTelefs t

--39
casa :: Nome -> Agenda -> Maybe Integer
casa nome ((n, cont):t) | nome == n =  aux nome cont
                        | otherwise = casa nome t
            where 
                aux nome [] = Nothing
                aux nome (h:t) = case h of
                    Casa a -> Just a
                    _ -> aux nome t

--40 (ex4)
type Dia = Int
type Mes = Int
type Ano = Int

data Data = D Dia Mes Ano deriving Show

type TabDN = [(Nome, Data)]

procura :: Nome -> TabDN -> Maybe Data
procura nome [] = Nothing
procura nome ((n,d):t) | nome == n = Just d
                       | otherwise = procura nome t
--41
idade :: Data -> Nome -> TabDN -> Maybe Int
idade dat nome [] = Nothing
idade dat nome ((n,d):t) | nome == n = Just (aux dat d)
                         | otherwise = idade dat nome t
            where 
                aux (D di me an) (D dia mes ano) | me == mes && dia> di = an - ano - 1
                                                 | me == mes && dia< di = an - ano
                                                 | me > mes = an - ano - 1
                                                 | me < mes = an - ano

--42
anterior :: Data -> Data -> Bool
anterior (D di me an) (D dia mes ano) | an < ano = True
                                      | an == ano && me < mes = True
                                      | an == ano && me == mes && di < dia = True
                                      | otherwise = False


--43
ordena :: TabDN  -> TabDN
ordena ((nome,d):t) | velho ((nome,d):t) = (nome,d) : ordena t
                    | otherwise = ordena (t ++ [(nome,d)])


velho :: TabDN -> Bool 
velho [(nome,d)] = True
velho ((nome,d):(nom, da):t) | anterior d da = velho ((nome,d):t)
                             | otherwise = False
                        
data Movimento = Credito Float | Debito Float deriving Show
data Data2 = G Int Int Int deriving Show
data Extracto = Ext Float [(Data2, String, Movimento)] deriving Show

--44
extValor :: Extracto -> Float -> [Movimento]
extValor (Ext a ((b, c, d):t)) e = case d of
                   Credito a -> if a > e then d : extValor (Ext a t) e else extValor (Ext a t) e
                   Debito a  -> if a > e then d : extValor (Ext a t) e else extValor (Ext a t) e


--45
filtro :: Extracto -> [String] -> [(Data2, Movimento)]
filtro (Ext a ((b, c, d):t)) (x:xs) = (aux x ((b, c, d):t)) ++ filtro (Ext a ((b, c, d):t)) xs
                                  

                    where
                        aux x [] = []
                        aux x ((b, c, d):t) | x == c = (b, d) : aux x t

--46 (Partion Eithers)
creDeb :: Extracto -> (Float,Float)
creDeb (Ext a []) = (0,0)
creDeb (Ext a ((b, c, d):t)) = case d of
    Credito a -> (a+x,y)
    Debito a -> (x,a+y)
    where
        (x,y) = creDeb (Ext a t)

--47

saldo :: Extracto -> Float
saldo (Ext a []) = a
saldo (Ext a ((b, c, d):t)) = case d of
    Credito e -> saldo (Ext (a-e) t)
    Debito e -> saldo (Ext (a-e) t)


--48 (Ficha 4)

digitAlpha :: String -> (String,String)
digitAlpha [] = ([],[])
digitAlpha (h:t) | isAlpha h = (h:x,y)
                 | isDigit h = (x, h:y)
                 | otherwise = digitAlpha t
        where 
            (x,y) = digitAlpha t
            isAlpha h | 64 < ord h && ord h < 91 || 96 < ord h && ord h < 123 = True
                      | otherwise = False
            isDigit h | 47 < ord h && ord h < 58 = True
                      | otherwise = False


--49
nzp :: [Int] -> (Int,Int,Int)
nzp [] = (0,0,0)
nzp (h:t) | h>0 = (x+1,y,z)
          | h == 0 = (x,y+1,z)
          | otherwise = (x,y,z+1)
        where
            (x,y,z) = nzp t

--50
divMod' :: Integral a => a -> a -> (a, a)
divMod' a b = acc a b 0

        where 

            acc a b c | abs a > abs b = acc ((abs a)-(abs b)) (abs b) (c+1)
                      | otherwise = ( c, a)

--51
fromDigits :: [Int] -> Int
fromDigits (h:t) = aux (h:t) (length t)
    where 
        aux [] c = 0
        aux (h:t) c = h*10^c + aux t (c-1) 

--52

--maxSumInit :: (Num a, Ord a) => [a] -> a
--maxSumInit (h:t) = acc (h:t)

--53
fib :: Int -> Int
fib n = acc n 0 1 
    where 
        acc 0 a b = 0
        acc 1 a b = a
        acc n a b = acc (n-1) b (a+b)

--54
intToStr :: Int -> String
intToStr a = aux a []
    where
        aux a l | a < 10 = [chr (a+48)] ++ l
        aux a [] = aux (div a 10) [chr ((resto a) + 48)]
        aux a l  = aux (div a 10) [chr ((resto a) + 48)]++ l
        resto a  = mod a 10

--55 
ex8 (h:t) = acc (h:t) [] 
     where 
         acc [] l    = l 
         acc (h:t) l | mod h 2 == 0 && mod h 3 == 0 = acc t (l++[h])
                     | otherwise = acc t l


ex8b []    = []
ex8b (h:t) | mod h 2 == 0 && mod h 3 == 0 = h : ex8b t
           | otherwise = ex8b t


ex8c [] _ =  []
ex8c (a:b) (h:t) = (dual a (h:t)) ++ ex8c b (h:t)
      where 
          dual a [] = []
          dual a (h:t) | a + h == 30 = [(a,h)]
                       | otherwise = dual a t


ex9a a = aux a 1 
     where
         aux a b  | a > b = b : aux a (2*b)
                  | otherwise = [b]

ex9b (a,b) =  aux (a,b) (a,b)
         where
             aux (a,b) (c,d) | a /= d = (c,d) : aux (a,b) (c+1,d-1)
                             | otherwise = [(c,d)]

                             
ex9c a = aux a 1
      where 
         aux a b | b <= a = (exp 1 b) : aux a (b+1)
                 | otherwise = []
         exp c b |  c < b = c : exp (c+1) b
                 | otherwise = [b]



ex9e a = aux a 1 2
    where
        aux 0 b c = []
        aux a b c = b : aux (a-1) (b*c) (c+1)

-- [ 2^x | x <- [0..10]]   
-- [ (x,y) | x <- [1..5], y <- [1..5], x+y == 6 ]  
-- [ [y | y <- [1..x]] | x <- [1..10]]  
-- [ [y | y <- ]]   
-- [ yx | y <-+1, x <- [1..10]]     

--56 (Ficha 5)
any' :: (a->Bool) -> [a] -> Bool
any' f [] = False
any' f (h:t) | f h = True
            | otherwise = any' f t

            
--57
zipWith' :: (a->b->c) -> [a] -> [b] -> [c]
zipWith' f _ [] = []
zipWith' f [] _ = []
zipWith' f (h:t) (a:b) = f h a : zipWith' f t b 

--58
takeWhile' :: (a->Bool) -> [a] -> [a] 
takeWhile' f [] = []
takeWhile' f (h:t) | f h = h : takeWhile' f t
                   | otherwise = []

--59
dropWhile' :: (a->Bool) -> [a] -> [a] 
dropWhile' f [] = []
dropWhile' f (h:t) | f h = dropWhile' f t
                   | otherwise = h:t 

--60...
span' :: (a->Bool) -> [a] -> ([a],[a])
span' f [] = ([],[])
span' f (h:t) | f h = (h:x,y)
              | otherwise = ([],(h:t))
    where
            (x,y) = span' f t

--61
deleteBy' :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy' f a [] = []
deleteBy' f a (h:t) | f a h = t
                    | otherwise = h : deleteBy' f a t

--62
sortOn' :: Ord b => (a -> b) -> [a] -> [a]
sortOn' f []    = []
sortOn' f (h:t) | menor f h t = h : sortOn' f t
                | otherwise = sortOn' f (t ++ [h])
          where
              menor f _ [] = True
              menor f h (x:xs) | f h <= f x = menor f h xs
                               | otherwise = False 
                               
                            
type Polinomio = [Monomio]
type Monomio = (Float,Int)
--63
selgrau2 :: Int -> Polinomio -> Polinomio 
selgrau2 x (h:t) = filter (\ (a,b) -> x == b ) (h:t)

conta2 :: Int -> Polinomio -> Int
conta2 n p = foldl (\ acc (a,b) -> if n == b then acc +1 else acc) 0 p

grau2 :: Polinomio -> Int
grau2 ((a,b):t) = foldl (\ acc (c,d) -> if d > b then d else acc ) b t

deriv2 :: Polinomio -> Polinomio 
deriv2 (h:t) = map (\ (a,b) -> (fromIntegral b*a,b-1)) (h:t)

calcula2 :: Float -> Polinomio -> Float
calcula2 x (h:t) = foldl (\ acc (a,b) -> (a*x)^b ) 0 (h:t)

simp2 :: Polinomio -> Polinomio 
simp2 (h:t) = filter (\ (a,b) -> a /= 0) (h:t)

mult2 :: Monomio -> Polinomio -> Polinomio
mult2 a ((x,xs):t) = map (\ (b,c) -> (x*b,xs+c)) ((x,xs):t)

ordena2 :: Polinomio -> Polinomio 
ordena2 (h:t) = sortOn snd (h:t)

normaliza2 :: Polinomio -> Polinomio 
normaliza2 (h:t) = aux (ordena (h:t))
      where 
         aux (h:t) = map( foldl (\ acc (a,b) -> if snd acc == b then acc ++ snd acc else acc ) h t) (h:t)

soma2 :: Polinomio -> Polinomio -> Polinomio
soma2 l1 l2 = ordena (normaliza l1) ++ ordena (normaliza l2)

produto2 :: Polinomio -> Polinomio -> Polinomio
produto2 l1 l2 = concat (map (\ m ->  mult2 m l1 ) l2)

equiv2 :: Polinomio -> Polinomio -> Bool
equiv2 l1 l2 = ordena(normaliza l1) == ordena(normaliza l2)

--3
type Mat a = [[a]]

dimOk :: Mat a -> Bool
dimOk (h:t) = all (\ a -> length h ==  length a) t


--4
dimMat :: Mat a -> (Int,Int)
dimMat [] = (0,0)
dimMat (h:t) = (length(h:t), length h)

--5 
addMat :: Num a => Mat a -> Mat a -> Mat a
addMat l1 l2 =  zipWith (\ a b -> zipWith (+) a b )  l1 l2

--6
transpose' :: Mat a -> Mat a
transpose' ([]:_) = []
transpose' l1 = map head l1 : transpose' (map tail l1)

--7
multMat' :: Num a => Mat a -> Mat a -> Mat a
multMat' l1 l2 =  sum (zipWith (\ a b -> a*b) l1 (transpose' l2))

--8
zipWhat :: (a -> b -> c) -> Mat a -> Mat b -> Mat c
zipWhat f l1 l2 = zipWith (\ a b -> zipWith f a b)  l1 l2


--10
rotateLeft' :: Mat a -> Mat a
rotateLeft' l1 = reverse (transpose l1)
            where
                reverse (h:t) = reverse t ++ 


