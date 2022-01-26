module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero
--PRIMERA PARTE

--1
data Categoria = Basica | Estandar | Premium
    deriving (Show, Eq)

data Pelicula = UnaPelicula {titulo:: String, duracion:: Number, genero:: String, origen:: String}
    deriving (Show, Eq)

data Usuario = UnUsuario {nombre:: String, categoria:: Categoria, edad:: Number, pais:: String, peliculasVistas::[Pelicula]}
    deriving (Show, Eq)

psicosis = UnaPelicula "Psicosis" 109 "Terror" "Estados Unidos"
pianista = UnaPelicula "El pianista" 150 "Drama" "Alemania" 
peliejemplo= UnaPelicula "aaaaaaaaaaa" 120 "Drama" "Alemania"
juan = UnUsuario "juan" Estandar 23  "Argentina" [ pianista ]
jose = UnUsuario "mathew" Premium 24 "Estados Unidos" [psicosis, pianista]

usuariosTotales = [juan, jose]
peliculasTotales = [psicosis, pianista, peliejemplo]

--2
verPelicula:: Usuario -> Pelicula -> Usuario
verPelicula usuario pelicula = usuario {peliculasVistas = (pelicula : peliculasVistas usuario)}

--3

premiar:: [Usuario] -> [Usuario]
premiar usuarios = filter esUsuarioInternacional usuarios

esUsuarioInternacional:: Usuario -> Bool
esUsuarioInternacional usuario = pais usuario /= "Estados Unidos"

-- 4 Criterios de busqueda
teQuedasteCorto:: [Pelicula] -> [Pelicula]
teQuedasteCorto lista = filter (\x -> duracion x < 35) lista

cuestionDeGenero:: String -> [Pelicula] -> [Pelicula]
cuestionDeGenero generoElegido lista = filter (\x -> genero x == generoElegido) lista

deDondeSaliste:: String -> [Pelicula] -> [Pelicula]
deDondeSaliste origenElegido lista = filter (\x -> origen x == origenElegido) lista

pelisSimilares:: Pelicula -> [Pelicula] -> [Pelicula]
pelisSimilares pelicula lista = filter (\x -> origen x == origen pelicula &&  largoTitulo x == largoTitulo pelicula && x /= pelicula) lista

largoTitulo:: Pelicula -> Number
largoTitulo = length.titulo

-- 5

peliculasNoVistas:: Usuario -> [Pelicula] -> [Pelicula]
peliculasNoVistas usuario listaPeliculas = filter (\x -> notElem x (peliculasVistas usuario) ) listaPeliculas

busquedaCriterios:: String -> Bool -> String -> Pelicula -> [Pelicula]
busquedaCriterios genero corto origen pelicula
    |corto =  teQuedasteCorto (pelisSimilares pelicula (deDondeSaliste origen (cuestionDeGenero genero peliculasTotales)))
    |otherwise = (pelisSimilares pelicula (deDondeSaliste origen (cuestionDeGenero genero peliculasTotales)))

recomendarTres:: Usuario -> String -> Bool -> String -> Pelicula -> [Pelicula]
recomendarTres usuario genero corto origen pelicula = take 3 (peliculasNoVistas usuario (busquedaCriterios genero corto origen pelicula))

--6 
recomendarTresNacionales:: Usuario -> [Pelicula]
recomendarTresNacionales usuario = take 3 (filter (\x -> genero x == genero (ultimaVista usuario)) (peliculasNoVistas usuario (pelisSimilares (ultimaVista usuario) peliculasTotales)))

ultimaVista =  head.peliculasVistas

--SEGUNDA PARTE

data UsuarioFanatico = UnUsuarioFanatico {nombreF:: String, categoriaF:: Categoria, edadF:: Number, paisF:: String, peliculasVistasF::[Pelicula], salud::Number}
    deriving (Show, Eq)

data Serie = UnaSerie {capitulos::[Capitulo]}
    deriving (Show, Eq)


data Alteracion = Indiferente | Leve | Nociva | Saludable
    deriving (Show, Eq)

catalina = UnUsuarioFanatico "catalina" Estandar 23  "Argentina" [ pianista ] 10
 
--1

data Capitulo = UnCapitulo {tituloC:: String, duracionC:: Number, generoC:: String, origenC:: String, alteracion::Alteracion}
    deriving (Show, Eq)

cap1 = UnCapitulo "capitulo 1" 30 "Comedia" "Estados Unidos" Leve
cap2 = UnCapitulo "capitulo 2" 30 "Comedia" "Estados Unidos" Saludable

theoffice = UnaSerie [cap1, cap2]

--2

consumirSerie:: UsuarioFanatico -> Capitulo -> UsuarioFanatico
consumirSerie (UnUsuarioFanatico a b c d e salud) capitulo 
    | alteracion capitulo == Saludable = (UnUsuarioFanatico a b c d e (salud+5))
    | alteracion capitulo == Leve = (UnUsuarioFanatico a b c d e (salud-2))
    | alteracion capitulo == Nociva = (UnUsuarioFanatico a b c d e (salud-5))
    | otherwise = (UnUsuarioFanatico a b c d e (salud))

--3 ejemplo
--consumirSerie catalina cap1 
--baja la salud de catalina 2 puntos 

--4 (no logre que se acumulen los efectos de ver los capitulos en el usuario fanatico)
maraton:: UsuarioFanatico -> Serie -> UsuarioFanatico 
maraton usuario serie = last (verListaDeCapitulos usuario (capitulos serie))

verListaDeCapitulos:: UsuarioFanatico -> [Capitulo] -> [UsuarioFanatico]
verListaDeCapitulos usuario [] = []
verListaDeCapitulos usuario lista= (consumirSerie usuario (head lista) : verListaDeCapitulos usuario (tail lista))

--5 Si la serie tuviese una infinita cantidad de capitulos, en este caso la funcion maraton no terminaria nunca

--6
--considerando una serie con una cantidad infinita de capitulos 

maratonSerieInfinita usuario serieInfinita cantidad = (verListaDeCapitulos usuario (take cantidad (capitulos serieInfinita)))
