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

-- Ejercicio 3

reparte :: [a] -> ([a], [a])
reparte [] = ([], [])
reparte [x] = ([x], [])
reparte (x:y:xs) = (x:xs1, y:xs2)
    where (xs1, xs2) = reparte xs
-- Ejercicio  4
distintos :: Eq a => [a] -> Bool 
dintintos [] = error " Lista vacía"
distintos [x] = True
distintos (x:xs) = not(elem x xs) && distintos (xs)

-- Ejercicio 11
--a)
take' :: Int -> [a] -> [a]
take' n xs = [ x | (p, x) <- zip [0..n-1] xs ]
--b)
drop' :: Int -> [a] -> [a]
drop' n xs = [x | (p, x) <- zip [0.. length xs] xs, p>=n]

-- Ejercicio 13
-- Compara la lista con la cola de la lista en pares y comprueba que el primer elemento es menor o igual al segundo guardando los resultados
-- (True o False) en una lista
-- La funcion and hace la conjunción de los elementos de la lista booleana entre sí --> True si todos los elementos son true y false si al menos uno es False.
desconocida :: (Ord a) => [a] -> Bool
desconocida xs = and [x<=y | (x, y) <- zip xs (tail xs)]
