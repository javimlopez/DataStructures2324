import Test.QuickCheck
import Data.Char
import DataStructures.Stack.LinearStack

-- Uso de let in
circArea :: Double -> Double
circArea r = pi * r^2

rectArea :: Double -> Double -> Double
rectArea b h = b*h

circLength :: Double -> Double
circLength r = 2 * pi * r

cylinderArea :: Double -> Double -> Double
cylinderArea r h =
    let
        circ = circArea r --definiciones locales sangradas al mismo nivel
        l = circLength r 
        rect = rectArea l h
    in 
        2*circ + rect --Expresión

-- Declaración de operadores
-- 4 -> Nivel de precedencia
-- infix = infijo no asociativo, infixl a la izq, infixr a la dcha
infix 4 ~= --Comprueba si dos reales son aprox iguales

(~=) :: Double -> Double -> Bool
x ~= y = abs (x-y) < epsilon
    where epsilon = 1/1000

-- Uso de definiciones locales
cylinderArea2 :: Double -> Double -> Double
cylinderArea2 r h = 2 * circ + rect
    where
        circ = circArea r
        l = circLength r 
        rect = rectArea l h

-- Uso de guardas
factorial :: Int -> Int
factorial x
 | x == 0 = 1
 | otherwise = x * factorial(x-1)
 
 -- Uso de tipos de Igualdad EQ, orden ORD
sign :: (Eq a, Ord a, Num a) => a -> a
sign x 
  | x>0 = 1
  | x<0 = -1
  | x == 0 = 0
-- Uso de aritmética fraccionaria
division :: (Fractional a) => a -> a -> a
division x y = x / y
-- Uso de aritmética entera
resto :: (Integral a) => a -> a -> a
resto x y = mod x y


-- Propiedades con QuickCheck
square :: (Num a) => a -> a 
square x = x * x 

p1 x y = True ==> square (x+y) == square x + square y + 2*x*y
p2 x y = True ==> abs (x+y) == abs x + abs y --Falla
p3 x y = x>=0 && y>=0 ==> abs (x+y) == abs x + abs y

-- Listas
--Primer elemento: head :: [a] -> a | Devuelve el primer elemento
--Lista sin el primer elemento: tail :: [a] -> [a]
-- ¿Es vacía? null :: [a] -> Bool
-- Añadir un elemento al principio -> Usar el constructor (:) O(1)
-- (:) asocia a la derecha
-- type String = [Char] Es una lista de caracteres 
-- ['p', 'e', 'p', 'e'] == "pepe" head "pepe" -> 'p'

-- Patrones
-- Definir funciones con diferentes casos (inverso a un constructor)

--Patrones para listas
-- [], [x], [x, y]

-- (x:xs), (x:y:xs)

-- longitud lista Prelude -> length O(n)
length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

--Comprobación lista ordenada
sorted :: (Ord a) => [a] -> Bool
sorted [] = True
sorted [_] = True
sorted (x:y:zs) = x <= y && sorted(y:zs)

--Concatenación de listas O(n)
-- Se usa el operador (++) predefinido asociativo a la derecha

--Inversión de listas lenta O(n^2)
--Utiliza la función predefinida reverse
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

--inversión de listas rápida
--Utiliza parámetros acumuladores (se acumula en revon)
reverse2 :: [a] -> [a]
reverse2 xs = revon xs []
    where
        revon [] ys = ys
        revon (x:xs) ys = revon xs (x:ys)

--Parámetros acumuladores
-- El segundo argumento es el índice del bucle
-- ac es el acumulador que acumula los valores en cada llamada
factorial' :: Integer -> Integer
factorial' x = aux 1 x
 where
    aux ac 0 = ac
    aux ac n | n>0 = aux (ac*n) (n-1)

--Sublistas

--take :: Int -> [a] -> [a]
-- take n xs devuelve el prefijo de xs de longitud n
-- si n es mayor que la longitud de xs devuelve xs
take' :: Int -> [a] -> [a]
take' x [] = []
take' 0 xs = []
take' x (y:ys) = y : take' (x-1) ys

--drop n xs devuelve el sufijo de xs después de los primeros n elementos
-- si n es mayor o igual que la longitud de xs devuelve la lista vacía
drop' :: Int -> [a] -> [a]
drop' x [] = []
drop' 0 xs = xs
drop' x (y:ys) = drop' (x-1) ys

--Funciones de orden superior
-- map :: (a -> b) -> [a] -> [b] es una función de orden superior
-- map aplica a cada elemento de la lista una función f
map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : map' f xs

--even comprueba si un número es par
even' :: (Integral a) => a -> Bool
even' x = mod x 2 == 0

--filter toma una condición y una lista y devuelve una lista
--La lista contiene todos los elementos para los q la condición es True
filter' :: (a -> Bool) -> [a] -> [a]
filter' p [] = []
filter' p (x:xs)
 | p x = x : filter' p xs
 | otherwise = filter' p xs

-- isDigit es una función de Char que comprueba si un caracter es un dígito o no
isDigit' :: Char -> Bool
isDigit' x = x >= '0' && x <= '9'

--funciones lamda
--Se puede pasar una función sin nombre
--map (\x -> x * x) [1,2,3]
--filter (\x -> mod x 2 == 0) [1,2,3,4]

--Secciones
--Aplicar un único argumento a un operador binario
--map (2*) [1,2,3] Hace 2*1, 2*2, 2*3 y lo mete en una lista
--filter (<3) [10, 2, 3, 1]

--Secuencias aritméticas
--Se escriben como [1..10] -> Dif 1
--[2,4,..10] --> Dif 2
--[1, 3..] Lista infinita impares

--Listas por comprensión
--Usan generadores: [expresion | patron <- lista] La flecha se lee pertenece a ..
--[x^2 | x <- [1..4]]
--[even x | x <- [1..4]]
--[(x, even x) | x <- [1..4]]

--Se pueden definir con guarda. Es una combinación entre map y filter
-- [expresion | patron <- lista, guarda ]
-- [ x | x <- [-1, 2, 3, -4], x>0] Solo tiene en cuenta los positivos
-- [ x | x <- [1,2,3,4], even x ] Solo tiene en cuenta los pares

-- Definiciones locales de listas por comprensión
-- [expresion1 | patron1 <- lista, let patron2 = expresion2]
-- [ (x,y) | x <- [1,2,3], let y = 2*x ] Let y se refiere al doble de x

-- Generadores múltiples
-- [ expresión | patrón1 <- lista1, patrón2 <- lista2, ... ]
-- [ (x,y) | x <- [1,2,3], y <- [10,20] ] Producto Cartesiano

-- Plegado de listas con foldr
-- Se pliegan dessde la derecha

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f z [ ] = z
foldr' f z (x:xs) = x `f` foldr' f z xs

add' :: (Num a) => a -> a -> a 
add' x y = x + y 

-- foldr' add 0 [1,2,3,4] -> 10

foldl' :: (b -> a -> b) -> b -> [a] -> b 
foldl' f z [] = z 
foldl' f z (x:xs) = foldl f (z `f` x) xs

-- Se usan plegados de listas en muchas funciones
sum' :: (Num a) =>[a] -> a 
sum' xs = foldl' (+) 0 xs 

product' :: (Num a) => [a] -> a 
product' xs = foldl' (*) 1 xs 

and' :: [Bool] -> Bool 
and' xs = foldr (&&) True xs 

--  Plegado de listas y expresiones lamda con múltiples argumentos
-- foldl' (\x y -> x^2 - y) 1 [1,2 .. 5]

-- Parcialización
f :: Int -> Int -> Int -> Int 
f x y z = x + 2*y + 3*z 

-- Esa función equivale a esta
-- Toma un entero x devolviendo la función que toma dos enteros y z
-- devolviendo el entero x + 2*y + 3*z
f' :: Int -> (Int -> Int -> Int)
f' = \x -> (\y z -> x + 2*y + 3*z)

--Otra definición
f'' :: Int -> (Int -> (Int -> Int))
f'' = \x -> (\y -> (\z -> x + 2*y + 3*z))

-- Después de cada aplicación, se obtiene una función con el resto de elementos´
-- f 10 tiene tipo Int -> Int -> Int
-- f 10 20 tiene tipo Int -> Int
-- f 10 20 30 tiene tipo Int
-- Puedes usar isMultipleOf 3 que es tipo Int -> Bool para
-- Poder usarlo en filter, ya que es el tipo de la función que
-- se le puede pasar
isMultipleOf :: Int -> Int -> Bool
isMultipleOf y x = mod x y == 0

-- Composición de funciones
-- Se hace con el operador infixr 9 .
-- (.) :: (b -> c) -> (a -> b) -> (a -> c)
-- f . g = \x -> f (g x)
isOdd :: Int -> Bool 
isOdd = not . even'

--Tipos algebraicos
-- Definición de nuevos tipos de datos usando data
-- Constructores de datos: Valores
data Direction = North | South | East | West

-- Las clases de tipos representan conjuntos abstractos de operaciones
-- Para implementar las operaciones de una clase en un tipo -> Instancia

-- Definir igualdad y desigualdad para el tipo direction

instance Eq Direction where
    North == North = True
    South == South = True
    East == East = True
    West == West = True
    _ == _ = False

-- Definir Ordenación para Direction

instance Ord Direction where
    North <= _ = True
    South <= North = False
    South <= _ = True
    East <= North = False 
    East <= South = False 
    East <= _ = True 
    West <= North = False 
    West <= South = False
    West <= East = False 
    West <= _ = True 

-- Definir Show para Direction
instance Show Direction where
    show North = "North"
    show South = "South"
    show East = "East"
    show West = "West"

-- Se pueden generar instancias automáticamente usando deriving

data Direccion = Norte | Sur | Este | Oeste deriving (Show, Eq, Ord)

--Tipos Unión: Varios constructores de datos con un componente

data Degrees = Celsius Double | Fahrenheit Double deriving Show 

frozen :: Degrees -> Bool
frozen (Celsius c) = c <= 0
frozen (Fahrenheit f) = f <= 32

toCelsius :: Degrees -> Degrees
toCelsius (Celsius c) = Celsius c
toCelsius (Fahrenheit f) = Celsius ((f-32) / 1.8)

-- Tipo unión polimórfico predefinido

data Eitherr a b = Left' a | Right' b

list' :: [Eitherr Int Bool]
list' = [Left' 1, Right' True, Left' 3, Left' 4]

--Tipo opcional polimórfico predefinido

data Maybee a = Nothing' | Just a 

--Tipos Producto
-- Un único constructor de datos con varios componentes

type Name = String
type Surname = String
type Age = Int 

data Person = Pers Name Surname Age deriving Show 

john :: Person
john = Pers "John" "Smith" 35

name :: Person -> Name 
name (Pers nm _ _) = nm 

surname :: Person -> Surname 
surname (Pers _ snm _) = snm 

age :: Person -> Age 
age (Pers _ _ ag ) = ag 

person :: Person -> String 
person x = show x

qSort :: (Ord a) => [a] -> [a]
qSort [] = []
qSort (h:xs) = qSort ys ++ [h] ++ qSort zs
 where
    ys = [x | x <- xs, x < h]
    zs = [x | x <- xs, x >= h]

--Stack

-- Ejemplo uso Stack
s1 :: Stack Int 
s1 = push 3 (push 2 (push 1 empty))

size :: Stack a -> Int 
size s
 | isEmpty s = 0
 | otherwise = 1 + size (pop s)


-- Axiomas con QuickCheck