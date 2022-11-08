

#Se extraen 3 cartas de un baraja española. P(una pareja)

#Regla de Laplace = nº casos favorables / nº casos totales (equiprobables)

#4 palos y 10 números por palo
#baraja = c(
 # paste("O", 1:10),
  #paste("C", 1:10),
  #paste("E", 1:10),
  #paste("B", 1:10)
#)

#OPCION 1

sim_case <- function(){
  baraja = paste(c("O", "C", "E", "B"), rep(1:10, each = 4))
  #sample : urna y cuantas cartas quiero sacar
  #sets::as.set() lo convierte en conjunto
  sets::as.set(sample(baraja, 3))   # -> choose(40,3) formas
  #combinaciones de 40 sobre 3. == kjlñ(40,3)
  #choose(40,3)
}

#table(baraja) te sale cuantos objetos hay de cada tipo

#Elijo un numero: ej= 3
#elijo dos palos: B ,O -> B 3 , O 3
#elijo una carta que no tenga el 3 -> setdiff

sim_fav_case <- function(){
  baraja = paste(c("O", "C", "E", "B"), rep(1:10, each = 4))
  
  numero = sample(1:10, 1)          # -> 10 formas
  palos = sample(c("O", "C", "E", "B"), 2)    # -> choose(4,2) formas
  
  #pareja = c(paste(palos[1] , numero), paste(palos[2], numero))
  pareja = paste(palos, numero)
  
  nueva_baraja = setdiff(baraja, paste(c("O", "C", "E", "B"), numero))
  carta3 = sample(nueva_baraja, 1)    # -> 36 formas
  
  #Lo ponemos en un conjunto por que no importa el orden.
  sets::as.set(c(pareja, carta3))
}


#LaPlace:


probabilidad = (10 * choose(4,2) * 36)/choose(40,3)



#OPCION 2:

#1) elegir un nº entre posibles -> 10 formas
#2) elegir dos palos para ese nº -> choose(4,2)
#3) elegir una carta que no tenga el mismo valor que la paraja -> 36



#Ahora nos importa el orden

#total
#variaciones(40,3)

#favorables
#10
#36 elijo la carta final
#






