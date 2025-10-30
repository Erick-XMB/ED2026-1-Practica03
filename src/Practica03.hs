module Practica03 where

-- Tipo de dato Prop
data Prop = 
    Var String |
    Cons Bool |
    Not Prop |
    And Prop Prop |
    Or Prop Prop |
    Impl Prop Prop |
    Syss Prop Prop 
    deriving (Eq)

-- Imprimir el tipo de dato Prop
instance Show Prop where
    show (Cons True) = "Verdadero"
    show (Cons False) = "Falso"
    show (Var p) = p
    show (Not p) = "¬" ++ show p 
    show (Or p q) = "(" ++ show p ++ " ∨ " ++ show q ++ ")"
    show (And p q) = "(" ++ show p ++ " ∧ " ++ show q ++ ")"
    show (Impl p q) = "(" ++ show p ++ " → " ++ show q ++ ")"
    show (Syss p q) = "(" ++ show p ++ " ↔ " ++ show q ++ ")"

-- Fórmulas proposicionales (Variables atómicas)
p, q, r, s, t, u :: Prop
p = Var "p"
q = Var "q"
r = Var "r"
s = Var "s"
t = Var "t"
u = Var "u"

-- Sinonimo para los estados
type Estado = [String]


--- Función auxiliar para la funcion auxiliar del ejercicio 1 ----
-- Esta funcion la definimos en la practica 2
--- Se usó para determinar si un elemento pertenece o no a una lista ---
isIn :: Eq a => a -> [a] -> Bool
isIn x [] = False 
isIn x [y] = if x == y then True else False
isIn x (y:ys) = if x == y then True else isIn x ys


-- Funcion auxiliar para que no se repitan elementos en el ejercicio 1
noRepetir :: Eq a => [a] -> [a] -- Recibe una lista de elementos "a" y devuelve una lista de lo mismo
                                -- Eq es para que sean comparables
noRepetir = noRepAux [] -- noRepetir llama la funcion auxiliar noRepAux y [] es una lista vacia
  where
    noRepAux _ [] = [] -- Si hay una lista vacia devolvemos la lista vacia
    noRepAux yaVistos (x:xs) = if (isIn x yaVistos) then (noRepAux yaVistos xs) else (x : noRepAux (yaVistos ++ [x]) xs)
    -- Usamos isIn pues asi podemos ver si un elemento ya esta en una lista
    -- aqui se ve que:
    -- en el if vemos si "x" ya ha sido visto antes
    -- Si esta en yaVistos, no se agrega a la lista resultante, y se sigue el proceso con el resto "xs"
    -- Si no esta en yaVistos, lo agregamos al resultado con ":" y se sigue con "xs"
    -- tambien se agrega x a la lista de yaVistos, asi no se puede volver a repetir


-- Ejercicio 1 devuelve el conjunto formado por todas las variables proposicionales que aparecen en f.
variables :: Prop -> [String]
variables (Var x) = [x] --Aqui se devuelve solo la variables x
variables (Cons _) = [] -- aqui como Cons es constante, y estas nos son variables entonces devolvemos una lista vacia
variables (Not p) = variables p
-- En estas buscamos las variables de cada formula, las concatenamos con ++ y usamos noRepetir para que no hayan repetidas
variables (And p q) = noRepetir (variables p ++ variables q)
variables (Or p q) =  noRepetir (variables p ++ variables q)
variables (Impl p q) = noRepetir (variables p ++ variables q)
variables (Syss p q) = noRepetir (variables p ++ variables q)


-- Ejercicio 2 Devuelve la interpretación de la fórmula f bajo i.
interpretacion :: Prop -> Estado -> Bool
-- i es de estados
interpretacion (Var x) i = isIn x i -- Las variables son verdad si aparece en el estado
interpretacion (Not p) i = not (interpretacion p i) -- usamos not para "invertir" el valor de la formula
--Usamos || y && para aplicarlas a las formulas
interpretacion (Or p q) i =  interpretacion p i || interpretacion q i
interpretacion (And p q) i =  interpretacion p i && interpretacion q i
-- En las dos siguientes usamos las equvalencias logicas de la Impl y Syss
interpretacion (Impl p q) i  = interpretacion (Not p) i || interpretacion q i
interpretacion (Syss p q) i = interpretacion (Impl p q) i && interpretacion (Impl q p) i


--Funcion auxiliar para el ejercicio 3 
-- Usamos el conjuntoPotencia que definimos en la practica pasada
conjuntoPotencia :: [a] -> [[a]]
conjuntoPotencia [] = [[]]
conjuntoPotencia (x:xs) = conjuntoPotencia xs ++ [(x:ys) | ys <- conjuntoPotencia xs] 


-- Ejercicio 3 devuelve todos los estados posibles con los que podemos evaluar la fórmula.
estadosPosibles :: Prop -> [Estado]
-- aqui obtenemos las variables y leugo usamos conjuntoPotencia para tener todos los estados
estadosPosibles p = conjuntoPotencia (variables p)


-- Ejercicio 4 devuelve la lista de todos los modelos deuna formula proposicional
modelos :: Prop -> [Estado]
-- Aqui primero recorremos todos los estados posibles, pero nos quedamos solo con lo que cumplen con "interpretacion"
modelos p = [i | i <- estadosPosibles p, interpretacion p i]


-- Ejercicio 5 dadas dos fórmulas φ1 y φ2 de la lógica proposicional, nos dice si φ1 y φ2 son equivalentes.
sonEquivalentes :: Prop -> Prop -> Bool
sonEquivalentes p q = checarEstados (estadosPosibles (Syss p q))
-- llamamos a la funcion de chacarEstados para ver que "Syss p q" es verdadera en todos los estados
    where   
        -- como es el caso base, si ya no hay estados que revisar entonces se va a deolver true
        checarEstados [] = True
        -- en este caso tomamos el primer estado de una lista de estados
        -- "interpretacion (Syss p q) i" nos dice si es verdad en ese caso, y si es asi volvemos a checar los estados que faltan
        -- si hay un estado donde no se cumple, entonces vamos a devolver "False"
        checarEstados (i:is) = if (interpretacion (Syss p q) i) then (checarEstados is) else (False)


-- Ejercicio 6 dada una fórmula proposicional, nos dice si es una tautología.
tautologia :: Prop -> Bool
-- Aqui comparamos todos los estados en que "p" es verdad con todos los posibles estados 
-- Si son iguales es porque es una tautologia, si no lo son, pues no es tautologia xd
tautologia p = modelos p == estadosPosibles p


-- Ejercicio 7 determina que la fórmula recibida es consecuencia lógica de la lista de fórmulas recibida.
consecuenciaLogica :: [Prop] -> Prop -> Bool
consecuenciaLogica [] q = tautologia q --si la lista de formulas esta vacia, q es consecuencia logica si es una tautologia
consecuenciaLogica [p] q = tautologia (Impl p q) -- si la lista solo tiene un elemento p, q es consecuencia si siempre pasa que p -> q
consecuenciaLogica (p:q:ps) s = consecuenciaLogica ((And p q):ps) s
-- si hay mas de una formula, combinamos las dos primeras con el And y seguimos evaluando el resto "ps"
-- s es la formula que queremos comprobar que sea la consecuencia logica de la lista de formulas