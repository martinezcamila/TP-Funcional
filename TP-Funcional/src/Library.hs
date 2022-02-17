module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero
--PRIMERA PARTE
type Criterio = Pelicula -> Bool
--1
data Categoria = Basica | Estandar | Premium
    deriving (Show, Eq)

data Pelicula = UnaPelicula {titulo:: String, duracion:: Number, genero:: String, origen:: String}
    deriving (Show, Eq)

data Usuario = UnUsuario {nombre:: String, categoria:: Categoria, edad:: Number, pais:: String, peliculasVistas::[Pelicula]}
    deriving (Show, Eq)

psicosis = UnaPelicula "Psicosis" 109 "Terror" "Estados Unidos"
resplandor = UnaPelicula "El Resplandor" 146 "Terror" "Estados Unidos"
alien = UnaPelicula "Alien" 116 "Terror" "Estados Unidos"
pianista = UnaPelicula "El pianista" 150 "Drama" "Alemania" 
historiaOficial = UnaPelicula "La Historia Oficial." 115 "Drama" "Argentina"
pistolaDesnuda = UnaPelicula "La Pistola Desnuda" 85 "Comedia" "Estados Unidos" 
esperandoLaCarroza = UnaPelicula "Esperando la Carroza" 96 "Comedia" "Argentina"
elSecretoDeSusOjos = UnaPelicula "Secreto de sus ojos," 127 "Drama" "Argentina"
elRobo = UnaPelicula "El Robo del Siglo..." 114 "Comedia" "Argentina"

juan = UnUsuario "juan" Estandar 23  "Argentina" [historiaOficial, pistolaDesnuda ]
jose = UnUsuario "mathew" Premium 24 "Estados Unidos" [psicosis, pianista, alien]

usuariosTotales = [juan, jose]
peliculasTotales = [psicosis, pianista, esperandoLaCarroza, resplandor, alien, pistolaDesnuda, historiaOficial, elSecretoDeSusOjos, elRobo]

--2
verPelicula:: Usuario -> Pelicula -> Usuario
verPelicula usuario pelicula = usuario {peliculasVistas = (pelicula : peliculasVistas usuario)}

--3

premiar:: [Usuario] -> [Usuario]
premiar usuarios = filter esUsuarioInternacional usuarios

esUsuarioInternacional:: Usuario -> Bool
esUsuarioInternacional usuario = pais usuario /= "Estados Unidos"

-- 4 Criterios de busqueda

teQuedasteCorto::  Pelicula -> Bool
teQuedasteCorto pelicula = duracion pelicula < 35

cuestionDeGenero:: String -> Pelicula -> Bool
cuestionDeGenero generoElegido pelicula = genero pelicula == generoElegido

deDondeSaliste:: String  -> Pelicula -> Bool
deDondeSaliste origenElegido pelicula = origen pelicula == origenElegido

pelisSimilares:: Pelicula -> Pelicula -> Bool
pelisSimilares pelicula1 pelicula2 = esUnaPeliculaSimilar pelicula1 pelicula2

esUnaPeliculaSimilar pelicula1 pelicula2 = 
    origen pelicula1 == origen pelicula2 && largoTitulo pelicula1 == largoTitulo pelicula2 && pelicula1 /= pelicula2

esNacional pelicula = origen pelicula == "Argentina"

esDeGeneroConocido usuario pelicula =  elem (genero pelicula)(generosVistos usuario)

largoTitulo:: Pelicula -> Number
largoTitulo = length.titulo

-- 5

peliculasNoVistas:: Usuario -> [Pelicula]
peliculasNoVistas usuario = filter (\x -> notElem x (peliculasVistas usuario)) peliculasTotales

ultimaVista =  head.peliculasVistas

generosVistos usuario = map genero (peliculasVistas usuario)


busquedaCriterios:: Usuario -> [Criterio] -> [Pelicula]
busquedaCriterios usuario listaDeCriterios = filter (cumpleTodosLosCriterios listaDeCriterios) (peliculasNoVistas usuario)

cumpleTodosLosCriterios listaDeCriterios elemento = all (\criterio -> criterio elemento ) listaDeCriterios

recomendarTres:: Usuario -> [Criterio] -> [Pelicula]
recomendarTres usuario criterios = take 3 (busquedaCriterios usuario criterios)

--6
busquedaCriteriosNacionales:: Usuario -> [Pelicula]
busquedaCriteriosNacionales usuario = filter (cumpleTodosLosCriterios [(esNacional), (esUnaPeliculaSimilar (ultimaVista usuario)), (esDeGeneroConocido usuario)]) (peliculasNoVistas usuario)

recomendarTresNacionales:: Usuario -> [Pelicula]
recomendarTresNacionales usuario = take 3 (busquedaCriteriosNacionales usuario)

--EJEMPLOS
--recomendarTres juan [not.teQuedasteCorto, (cuestionDeGenero "Terror"), (deDondeSaliste "Estados Unidos")] -> devuelve las peliculas Alien, El Resplandor y Psicosis
--recomendarTresNacionales juan --> devuelve las peliculas Esperando la Carroza, El Secreto de sus Ojos y El Robo del Siglo

--SEGUNDA PARTE

data UsuarioFanatico = UnUsuarioFanatico {nombreF:: String, categoriaF:: Categoria, edadF:: Number, paisF:: String, peliculasVistasF::[Pelicula], salud::Number}
    deriving (Show, Eq)

data Serie = UnaSerie {capitulos::[Capitulo]}
    deriving (Show, Eq)


catalina = UnUsuarioFanatico "catalina" Estandar 23  "Argentina" [ pianista ] 10
 
--1

data Capitulo = UnCapitulo {tituloC:: String, duracionC:: Number, generoC:: String, origenC:: String, alteracion::Number}
    deriving (Show, Eq)

cap1 = UnCapitulo "capitulo 1" 30 "Comedia" "Estados Unidos" 5
cap2 = UnCapitulo "capitulo 2" 30 "Comedia" "Estados Unidos" (-10)

theoffice = UnaSerie [cap1, cap2]

--2
consumirSerie usuarioFanatico capitulo = usuarioFanatico {salud = (salud usuarioFanatico + alteracion capitulo )}


--3 ejemplo
--consumirSerie catalina cap1 
--sube la salud de catalina 5 puntos 

--4 
maraton:: UsuarioFanatico -> Serie -> UsuarioFanatico 
maraton usuarioFanatico serie = usuarioFanatico {salud = (salud usuarioFanatico + acumulacionDeAlteracion (capitulos serie) )}

acumulacionDeAlteracion:: [Capitulo] -> Number
acumulacionDeAlteracion [] = 0
acumulacionDeAlteracion (x:xs) = (alteracion x) + acumulacionDeAlteracion xs

--5 Si la serie tuviese una infinita cantidad de capitulos, en este caso la funcion maraton no terminaria nunca

-- --6
--tomando una serie con una cantidad infinita de capitulos pero solo considerando una cantidad especifica de esos capitulos
maratonSerieInfinita usuario serie cantidadCapitulos = (usuario {salud = (salud usuario + acumulacionDeAlteracion (take cantidadCapitulos (capitulos serie)) )})

-- maratonSerieInfinita usuario serieInfinita cantidad = (verListaDeCapitulos usuario (take cantidad (capitulos serieInfinita)))
