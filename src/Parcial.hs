data Perro = unPerro {
    raza :: String
    juguetesFavs :: [String]
    tiempoAPermanecer :: Int
    energia :: Float 
} 
type RazasExtravagantes = [String]
jugar :: Perro -> Perro
jugar unPerro  = unPerro{energia - max (energia-10) 0}

ladrar :: Float->Perro  -> Perro
ladrar ladridos unPerro  = unPerro{energia = energia + ladridos/2}

regalar ::  String-> Perro-> Perro 
regalar unJuguete unPerro  = agregarJuguete unPerro unJuguete

agregarJuguete :: Perro -> String -> Perro
agregarJuguete unPerro unJuguete = unPerro{juguetesFavs =  unPerro : juguetesFavs}

diaDeSpa :: Perro -> Perro
diaDeSpa unPerro| esRazaExtravagante || PermaneceMasDe50Min = agregarJuguete unPerro{energia=100} "Peine De Goma"

esRazaExtravagante:: Perro -> RazasExtravagantes -> Bool
esRazaExtravagante unPerro listaRazasExtravagantes = elem (energia unPerro) listaRazasExtravagantes 

permaneceMasDe50Min :: Perro -> Bool
permaneceMasDe50Min unPerro = (tiempoAPermanecer unPerro) > 50

diaDeCampo :: Perro -> Perro
diaDeCampo unPerro= jugar unPerro{juguetesFavs = drop 1 juguetesFavs} 

Zara :: Perro 
Zara = unPerro "Dalmata" ["Pelota","Mantita"] 90 80
type Ejercicio = Perro -> Perro 
type Duracion = Float
type Rutina = [(Ejercicio , Duracion)]

data Guarderia = unaGuarderia{nombre :: String
                  rutina ::Rutina}

guarderiaPdePerritos :: PerroGuarderia 
guarderiaPdePerritos = unaGuarderia "GuarderíaPdePerritos" [(jugar,30), (ladrar 18 ,20),(regalar "Pelota",0),(diaDeSpa ,120 ),(diaDeCampo ,720)]



puedeQuedarse :: Perro -> Guarderia -> Bool 
puedeQuedarse unPerro unaGuarderiaguarderia  = (tiempoAPermanecer unPerro) > sum (tiempoDeRutina (unRutina unaGuarderia))

tiemposDeRutina::Rutina -> Duracion
tiempoDeRutina [(_,duracion)]= duracion
ejerciciosDeRutina :: Rutina -> Ejercicio
ejerciciosDeRutina [(ejercicio,_)]= ejercicio
perrosResponsables :: Guarderia -> Bool
perrosResponsables unaGuarderia = any huboDiaDeCampo 
ejerciciosDeRutina (rutina unaGuarderia)
