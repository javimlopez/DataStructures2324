-------------------------------------------------------------------------------
-- Estructuras de Datos. 2º Curso. ETSI Informática. UMA
--
-- (completa y sustituye los siguientes datos)
-- Titulación: Grado en Ingeniería del Software.
-- Alumno: MOTA LOPEZ, FRANCISCO JAVIER
-- Fecha de entrega: DIA | MES | AÑO
--
-- Relación de Ejercicios 1. Ejercicios resueltos: ..........
--
-------------------------------------------------------------------------------
import Test.QuickCheck
import Data.Char

--1 (Resuelto) Tres enteros positivos x, y, z constituyen una terna pitagórica si x^2+y^2=z^2, es decir, si son los lados de un triángulo rectángulo.
--a) (Resuelto) Define la función que compruebe si tres valores forman una terna pitagórica.
esTerna :: Integer -> Integer -> Integer -> Bool
esTerna x y z
    | (x^2)+(y^2) /= (z^2) = False
    | otherwise = True

--b) (Resuelto) Es fácil demostrar que para cualesquiera x e y enteros positivos con x>y, la terna (x2-y2, 2xy, x2+y2) 
--es pitagórica. Usando esto, escribe una función terna que tome dos parámetros y devuelva una terna pitagórica. 
terna :: Integer -> Integer -> (Integer, Integer, Integer)
terna x y 
    | x <= y = error "No es una terna pitagórica"
    | otherwise = (x^2-y^2, 2*x*y, x^2+y^2)

--c) y d) (Resuelto). Lee y entiende la siguiente propiedad, para comprobar que todas las ternas generadas por la 
--función terna son pitagóricas:
--Si x es mayor que cero,  y es mayor que cero y x es mayor que y, entonces para un valor c (a,b,c) es una terna pitagórica
p_ternas x y = x>0 && y>0 && x>y ==> esTerna a b c
    where (a, b, c) = terna x y

--2 (Resuelto) Define una función polimórfica que intercambie de posición los datos de la tupla:
intercambia :: (a, b) -> (b, a)
intercambia (a,b) = (b,a)

--3 (Resuelto) Este ejercicio versa sobre ordenación de tuplas.
--a) (Resuelto) Define una función sobrecargada para tipos con orden que tome una tupla con dos valores del mismo tipo y la devuelva ordenada de menor a mayor:
ordena2 :: Ord a => (a, a) -> (a, a)
ordena2 (x,y) 
    | x<y = (x,y)
    | otherwise = (y,x)

--Copia en tu fichero las siguientes propiedades relativas a la función ordena2:
--Entiende lo que cada una significa, y compruébalas usando QuickCheck
p1_ordena2 x y = enOrden (ordena2 (x,y))
    where enOrden (x, y) = x<=y

p2_ordena2 x y = mismosElementos (x, y) (ordena2 (x,y))
    where
     mismosElementos (x, y) (z, v) = (x==z && y==v) || (x==v && y==z)

--b) (Resuelto) Define una función sobrecargada para tipos con orden que tome una tupla con tres valores 
--del mismo tipo y la devuelva ordenada, con los elementos de menor a mayor:
ordena3 :: Ord a => (a, a, a) -> (a, a, a)
ordena3 (x, y, z) 
    | y < x = ordena3(y, x, z)
    | y > z = ordena3(x, z, y)
    | otherwise = (x, y, z)

--c) (Resuelto). Escribe propiedades análogas a las del apartado anterior pero para esta función, y compruébalas usando QuickCheck
p1_ordena3 x y z = enOrden (ordena3 (x, y, z))
    where enOrden (x, y, z) = x<=y && y<=z

p2_ordena3 x y z = mismosElementos (x, y, z) (ordena3 (x, y, z))
    where
     mismosElementos (x, y, z) (a, b, c) = (x==a && y==b && z==c) || (x==a && y==c && z==b) || (x==b && y==a && z==c) || (x==b && y==c && z==a) || (x==c && y==a && z==b) || (x==c && y==b && z==a)


--4  (Resuelto) Aunque ya existe una función predefinida para calcular el 
--máximo de dos valores, el objetivo de este ejercicio es que definas tu propia versión de dicha función.
--a) (Resuelto) Como no está permitido redefinir una función predefinida, define una nueva y llámala max2 a de forma que satisfaga: 
max2 :: Ord a => a -> a -> a
max2 x y
    | x>y = x
    | otherwise = y

--b) (Resuelto) Define las siguientes propiedades que debería verificar tu función max2 y compruébalas con 
--QuickCheck (recuerda importar Test.QuickCheck al principio de tu programa):

--El maximo de dos números x e y coincide o bien con x o bien con y
p1_max2 x y = max2 x y == x || max2 x y == y
--El maximo de x e y es mayor o igual que x así como mayor o igual que y
p2_max2 x y = max2 x y >= x && max2 x y >= y
--Si x es mayor o igual que y, entonces el máximo de x e y es x.
p3_max2 x y = x >= y ==> max2 x y == x
--Si y es mayor o igual que x, entonces el máximo de x e y es y
p4_max2 x y = y >= x ==> max2 x y == y


--5 (Resuelto) Define una función sobrecargada para tipos con orden que tome un valor x además de una tupla con dos valores (min,max) y compruebe si x pertenece al 
--intervalo determinado por min y max, es decir, si x ∈ [min,max], devolviendo True o False según corresponda. 
entre :: Ord a => a -> (a, a) -> Bool
entre x (y, z)
    | x<y = False
    | x>z = False
    | otherwise = True


--6 (Resuelto) Define una función sobrecargada para tipos con igualdad que tome una tupla con tres valores del mismo tipo y devuelva True si todos son iguales  
iguales3 :: Eq a => (a,a,a) -> Bool
iguales3 (x, y, z)
    | x==y && x == z = True
    | otherwise = False

--7 (Resuelto) Recuerda que el cociente y el resto de la división de enteros se corresponde con las funciones predefinidas div y mod. 

--a) (Resuelto) Define una función descomponer que, dada una cantidad positiva de segundos, devuelva la 
--descomposición en horas, minutos y segundos en forma de tupla, de modo que los minutos y 
--segundos de la tupla estén en el rango 0 a 59
type TotalSegundos = Integer
type Horas = Integer
type Minutos = Integer
type Segundos = Integer
descomponer :: TotalSegundos -> (Horas, Minutos, Segundos)
descomponer x = (horas, minutos, segundos)
    where
     horas = (div x 3600) 
     minutos = div (mod x 3600) (60) 
     segundos = mod(mod x 3600) (60) 

--b) (Resuelto) Comprueba la corrección de tu función verificando con QuickCheck que cumple la siguiente propiedad: 
p_descomponer x = x>=0 ==> h*3600 + m*60 + s == x && entre m (0, 59) && entre s (0, 59)
    where
     (h, m, s) = descomponer x

--8 (Resuelto) Sea la siguiente definición que representa que un euro son 166.386 pesetas

--a) (Resuelto) Define una función pesetasAEuros que convierta una cantidad (de tipo Double) de pesetas en los correspondientes euros. 
unEuro :: Double
unEuro = 166.386
pesetasAEuros :: Double -> Double
pesetasAEuros x = x/unEuro

--b) (Resuelto) Define la función eurosAPesetas que convierta euros en pesetas.
eurosAPesetas x = x*unEuro

--c) (Resuelto) Sea la siguiente propiedad, que establece que si pasamos una cantidad de pesetas a euros y los 
--euros los volvemos a pasar a pesetas, obtenemos las pesetas originales (es decir, que las funciones
--definidas previamente son inversas)
p_inversas :: Double -> Bool
p_inversas x = eurosAPesetas (pesetasAEuros x) ~= x

--9 (Resuelto) Sea el siguiente operador que comprueba si dos valores de tipo Double son aproximadamente iguales
infix 4 ~= 
(~=) :: Double -> Double -> Bool
x ~= y = abs (x-y) < epsilon
    where epsilon = 1/1000

--10 (Falta corregirlo) Consideremos la ecuación de segundo grado ax2 + bx + c = 0. 
--a) (Resuelto)  Define una función raíces que tome tres parámetros (correspondientes a los coeficientes a, b y c
--de la ecuación) y devuelva una tupla con las dos soluciones reales de la ecuación (para calcular la 
--raíz cuadrada, usa la función predefinida sqrt). Recuerda que el discriminante se define como 
--b2-4ac y que la ecuación tiene raíces reales si el discriminante no es negativo.
raices :: Double -> Double -> Double -> (Double, Double)
raices a b c 
    | a == 0 = error "La ecuación no es de segundo grado"
    | determinante < 0 = error "La ecuación no tiene soluciones reales"
    | otherwise = (((-b) + (sqrt determinante))/2*a, ((-b) - (sqrt determinante))/ (2*a))
        where
        determinante = b^2-(4*a*c)

--b) (Falta corregirlo) Sea la siguiente propiedad que comprueba que las valores devueltos por la función raíces son efectivamente raíces de la ecuación:
p1_raices a b c = esRaíz r1 && esRaíz r2
 where
 (r1,r2) = raices a b c
 esRaíz r = a*r^2 + b*r + c ~= 0

--Comprueba esta propiedad con QuickCheck y verifica que falla. Piensa por qué falla, y añade 
--condiciones la propiedad para que no falle, es decir, completa las interrogaciones de forma que se verifique el siguiente diálogo:
p2_raices a b c = a==b && b==a ==> esRaíz r1 && esRaíz r2
 where
 (r1,r2) = raices a b c
 esRaíz r = a*r^2 + b*r + c ~= 0


--11 (Resuelto). . Define una función esMúltiplo sobrecargada para tipos integrales que tome dos valores x e y, y devuelva True si x es múltiplo de y. 
esMultiplo :: Integer -> Integer -> Bool
esMultiplo x y
    | mod y x /= 0 = False
    | otherwise = True


--12 (Resuelto). Define el operador de implicación lógica (==>>) :: Bool -> Bool -> Bool de forma que sea 
--asociativo a la izquierda, con precedencia menor que los operadores conjunción y disyunción:
infix 1 ==>>
(==>>) :: Bool -> Bool -> Bool
True ==>> x = x
False ==>> x = True


--13 (Pendiente de revisión). Los años bisiestos son los años múltiplos de 4. Una excepción a esta regla son los años múltiplos de 
--100, que sólo se consideran bisiestos si además son múltiplos de 400. Define una función 
--esBisiesto que tome como parámetro un año y devuelva True si es bisiesto.

--Ayuda: utiliza el operador de implicación lógica y la siguiente frase: “n es bisiesto si satisface las dos 
--condiciones siguientes: (a) es múltiplo de 4, y (b) si n es múltiplo de 100 entonces n es múltiplo de 
--400”
esBisiesto :: Integer -> Bool
esBisiesto x
    | mod x 4 /= 0 = False
    | mod x 100 == 0 && mod x 400 /= 0 = False
    | otherwise = True




--14 (Falta apartado d). Aunque ya existe en Haskell el operador predefinido (^) para calcular potencias, el objetivo de este 
--problema es que definas tus propias versiones recursivas de este operador.

--a) (Resuelto) A partir de la propiedad bn = b bn-1 define una función recursiva potencia que tome un entero b
--y un exponente natural n y devuelva bn
potencia :: Integer -> Integer -> Integer
potencia x y = if y == 0 then 1 else x * potencia x (y-1)

--b) (Resuelto) A partir de la propiedad bn = b bn-1 define una función recursiva potencia que tome un entero b
--y un exponente natural n y devuelva bn
potencia' :: Integer -> Integer -> Integer
potencia' x y
    | y==0 = 1 
    | y==2 = x * x
    | mod y 2 == 0 = potencia' (potencia' x (div y 2)) 2
    | otherwise = x * potencia' (potencia' x (div (y-1) 2)) 2
    
--c) (Resuelto) A partir de la siguiente propiedad define (sin usar la función del apartado anterior) una función recursiva potencia' que tome un 
--entero b y un exponente natural n y devuelva bn
p_pot b n = n>=0 ==> potencia b n == sol
 && potencia' b n == sol
 where sol = b^n

 --d) (Pendiente de hacer) Teniendo en cuenta que elevar al cuadrado equivale a realizar un producto, determina el número 
--de productos que realizan ambas funciones para elevar cierta base a un exponente n.
--Ayuda: para analizar la eficiencia de potencia' considera exponentes que sean potencia de 2


--15 Resuelto.  Escribe una función factorial que tome como parámetro un número natural y devuelva su factorial. 
--Dado que el factorial crece muy rápido, usa el tipo Integer
factorial :: Integer -> Integer
factorial x
    | x == 0 = 1
    | x == 1 = 1
    | otherwise = x * factorial (x - 1)


--16 (Pendiente de resolver el c). Este ejercicio estudia la división entera (exacta) de números enteros.
--a) (Resuelto). Define una función divideA que compruebe si su primer argumento divide exactamente al segundo
divideA :: Integer -> Integer -> Bool
divideA y x
    | y == 0 = error "No se puede dividir por cero"
    | mod x y == 0 = True
    | otherwise = False

--b) (Resuelto) Lee, entiende y comprueba con QuickCheck la siguiente propiedad referente a la función divideA
p1_divideA x y = y/=0 && y `divideA` x ==> div x y * y == x

--c) (Por resolver) Escribe una propiedad p2_divideA para comprobar usando QuickCheck que si un número divide a 
--otros dos, también divide a la suma de ambos.
p2_divideA x y h = y/=0 && y `divideA` x && y `divideA` h ==> div (x+h) y * y == x + h


--17 (Resuelto). La mediana de un conjunto de valores es aquel valor tal que el 50% de los valores del conjunto son 
--menores o iguales a él, y los restantes mayores o iguales. Queremos definir una función para 
--calcular la mediana de los valores de una tupla de cinco elementos
mediana :: Ord a => (a,a,a,a,a) -> a
mediana (x,y,z,t,u)
 | x > z = mediana (z, y, x, t, u)
 | y > z = mediana (x, z, y, t, u)
 | t < z = mediana (x, y, t, z, u)
 | u < z = mediana (x, y, u, t, z)
 | otherwise = z