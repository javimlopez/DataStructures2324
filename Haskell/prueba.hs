-- tail call optimization

sumaTotal x = if x == 0
    then 0
    else x + sumaTotal (x - 1)



-- crear lista funcionalmente [0, 2, 3, 4, 5]s
crearLista a b = if a == 0
    then b
    else 
        crearLista z(z:b)
            where z = a-1

-- pipe operator para componer funciones
duplicarLista:: [Int] -> [Int]
duplicarLista = map (* 2)

-- Filtrar lista
filtrarLista:: [Int] -> [Int]
filtrarLista = filter (\x -> (mod x 2) == 0)

-- Incrementar Lista

incrementarLista:: [Int] -> [Int]
incrementarLista = map succ

-- Combinación de 3 funciones la primera que se ejecuta es la última que se escribe (pipe operator)
combinar = incrementarLista . duplicarLista . filtrarLista

-- currying (Repasar)

sumarTresNumeros x y z = x + y + z

sumarYMostrarPrivate driver x y  = driver (show (x + y))    

sumarYMostrar = sumarYMostrarPrivate putStrLn

-- pattern matching
imprimirNumeroSuerte:: Int -> String
imprimirNumeroSuerte 7 = "Numero de la suerte!!!"
imprimirNumeroSuerte 5 = "Otro buen número"
imprimirNumeroSuerte x = "Sigue participando"

sumaTotalOpt :: Int -> Int -> Int
sumaTotalOpt 0 y = y
sumaTotalOpt x y = sumaTotalOpt (x-1) (x + y)

-- deconstructing (Evitar los if else)

data Peso = PesoEnKg Float | PesoEng Float

mostrarPesoKg:: Peso -> IO ()
mostrarPesoKg (PesoEnKg x) = putStrLn(show x)
mostrarPesoKg (PesoEng x) = putStrLn(show (x/1000))

-- Mis pruebas
miFactorial :: Integer -> Integer
miFactorial(0) = 1
miFactorial(n) = miFactorial(n-1)*n

--Asignar un tipo numérico

suma x y = (x :: Integer) + (y :: Integer)

divison :: Float -> Float -> Float
divison x y = x/y

discriminador :: Char -> Bool
discriminador x = if (isUpper(x) == False) && (isLower(x) == False) then True else False