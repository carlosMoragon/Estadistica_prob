#Ejercicio:

#5 amigos
#3 cafes y 2 cañas



#primero repartimos los cafes -> chosse(5,3)
#ponemos a los 2 restantes las cañas

#algoritmo:(combinaciones)
#salida: quienes s toma el cafe
c("cana", "cafe", "cafe", "cafe", "cana")


#algoritmo: (variaciones con repeticion)
#posicion 1 para tino , 2 para gauss, 3 posicion euler
c("cana", "cafe", "cafe", "cafe", "cana")
#orden importa -> Si
#si hay elementos repetidos -> Si
#5!/3!*2!

  
  
#Ejercicio:

#3 espadas y 2 copas

#casos totales
total<-function(){
  baraja = paste(c("O", "C", "E", "B"), rep(1:10, each = 4))
  sets::as.set(sample(baraja, 5))   # -> choose(40, 5)
}

fav<-function(){ # n_fav = choose(10,2) * choose(10,3)
  #primero sacamos 2 copas -> choose(10, 2)
  copas = sample("C", 1:10, 2)
  #sacamos 3 espadas -> choose(10,3)
  espadas = sample("E", 1:10, 3)
  
  sets::as.set(c(copas, espadas))
}


Laplace = (choose(10,2) * choose(10,3))/choose(40,5)


#Ejercicio:

#6 butacas
#3 chicas y 3 chicos
#no 2 chicos juntos ni 2 chicas juntas

#elijo o chica o chico

#elijo chico:
#puedo sentar a 3 chicos
#elijo chica, se sienta hay 3 chicas
(3*2*1*3*2*1)*2

#1) elijo sexo
#2) reordeno chicos alazar
#3) reordeno chicas alazar


#Ejercicio:

#Sacar 25 caras en 50 tiradas

#numero de posibilidades totales

#totales
total = 2^50 

#favorables
fav = choose(50,25) #seleccion de donde vana a estar las caras.


probabilidad = fav / total

#sample por defecto hace muestreo sin reemplazamiento sample(1:10, 2) del 1:10 saca 2 pero en la urna se quedan 8
#muestreo con reemplazamiento sample(1:10, 50, replace = TRUE) saca uno mira el valor y lo devuelve a la urna
casosTotales = sample(c("X", "C"), 50, replace = TRUE) #variaciones con repeticion.

trial = rep("X", 50)
indice = sample(1:50, 25)   #--> no importa el orden asi que combinaciones(50, 25) -> choose(50, 25)
trial[indice] = "C"



#Ejercicio:
#  10 parejas de hermanos se han apuntado a f´utbol. Si se hace un equipo de 8
#personas al azar, cu´al es la probabilidad de que no haya hermanos en el equipo.


participantes = paste(rep(1:10, each = 2), c("H", "M"))

#totales

sets::as.set(sample(participantes, 8))  #-> choose(20, 8)


#favorables

sample(1:10,8)  #-> choose(10, 5)
sample(1:2, 8, replace = TRUE) #-> 2^8
solucion = 2^8 * choose(10,8)/choose(20,8)




#SIMULACION

#25 caras en 50 tiradas de moneda


sim_all_cases = function(){
  sample(c("X", "C"), 50, replace = TRUE)
}

n = 50000
simulations = replicate(n, {
  event = sim_all_cases()
  #Comprobar si es un caso exitoso(25 caras)
  nb_caras = sum(event == "C")
  nb_caras == 25
})


sum(simulations)/n


#3 rojas del 1-3 y 3 bolas del 1-3. sacamos 2 bolas P(dos 3)
n = 5000
simulations = replicate(n, {
  urna = rep(1:3, 2)
  #si todos son verdaderos AND
  #el inverso de all() es any() es una OR
  all(sample(urna, 2) == 3)
  
})
sum(simulations)/n

#lo de antes o una negra y una roja.
n = 5000
simulations = replicate(n, {
  urna = paste(1:3, c("R", "N"))
  bolas = sample(urna, 2)
  numeros = sapply(strsplit(bolas, " "), function(vec) vec[1])
  colores = sapply(strsplit(bolas, " "), function(vec) vec[2])
  
  sets::as.set(colores) == sets::as.set(c("R", "N")) || sets::as.set(numeros) == sets::as.set(c(3, 3))
})

sum(simulations)/n


#pareja de una baraja
n = 5000
simulations = replicate(n,{
  baraja = paste(1:10, rep(c("B", "C", "O", "E"), each = 10))
  trial = sample(baraja, 3)
  #te devuelve una lista de vectores -> strsplit(trial, " ")
  
  #forma 1:
  sapply(strsplit(trial, " "), function(vec) vec[1])
  
  #forma 2:
  #map devuelve una lista de vectores. map_ nos devuelve lo que qramos en este caso chars
  ns = purrr::map_chr(strsplit(trial, " "), 1)
  
  #opcion 1:
  length(table(ns)) == 2
})

sum(simulations)/n
#opcion 2: Preparacion para el futuro jeje

counts = table(ns)
(length(counts) == 2) && (all(counts == 1:2) || all(counts == 2:1))



