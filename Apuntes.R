#'
#'APUNTES R: CARLOS MORAGÓN CORELLA
#'
#'
#'Definir variables hay 2 opciones:
primera = 3
segunda <- 3

#'No existe tipos de variable, R lo interpreta todo como vectores.
#'
#'Para crear vectores hay varias opciones:
#'
#'Los ":" solo se pueden utilizar para vectores numéricos:
#'Ejemplo del 1 al 10:
my_vector = 1:10

#'Tambien se puede emplear el metodo c(), de concatenar:
#'Ejemplo del 1 al 10:
my_vector_2 = c(1,2,3,4,5,6,7,8,9,10)

#'Metodos utiles:
#'
#'Sumar todos los numero de un vector:
suma = sum(my_vector)

#'Realizar una secuencia:
#'Ejemplo del 2 al 10, con saltos de 2 (es decir los pares)
pares = seq(2, 10, by = 2)
#'Ejemplo del 2 al 10, con saltos de 3 (es decir los impares)
impares = seq(3, 10, by = 3)

#'Repetir una variable un número de veces:
repeticion = rep("Gauss", 5)

#'Crear vectores random:
random1 = runif(3)
random2 = rnorm(3)


#'Para importar un fichero .csv y trabajar con sus datos, le damos al fichero, y le damos a import.
#'Si continuamos los pasos nos va a salir este código:
library(readr) #'importamos la libreria
covid19 <- read_csv("covid19.csv") #'metemos el fichero en una variable, transformandose en un dataframe
View(covid19) #'Para que nos aparezca la tabla en otro fichero que podamos ver.


#'Tratamiento de los datos:
#'
#'Coger solo ciertas columnas:
#'Por nombres:
newCovid = covid19[, c("day", "month", "year", "cases", "deaths", "countriesAndTerritories", "Cumulative_number_for_14_days_of_COVID.19_cases_per_100000")]
#'Tambien se puede hacer por números.
#'
#'Cambiar el nombre a ciertas columnas:
#'Ejemplo a la 6 y a la 7:
colnames(newCovid)[6:7] = c("country", "cum_rate") #'colnames nos da un vector con los nombres de las columnas

#'Añadir una columna a la tabla:
newCovid$time = newCovid$day + (newCovid$month - 1) * 30 + (newCovid$year - 2019) * 365

#'Forma de obtener la formula en vez de estar poniendo el $. Metodo with, donde se informa de donde son las variables.
#with(
#  newCovid,
#  day + (month - 1) * 30 + (year - 2019) * 365
#  )

#'Eliminiar una columna:
newCovid$day = NULL
newCovid = newCovid[, -1:-2] #' -1 elimina la primera columna, -2 la segunda, etc.


#'Condicionales en R:
notes = c(10, 9, 8, 4, 0) #'Notas de la clase.

notes > 5 #devuelve un vector de true y false

sum(notes > 5) #transforma los true a 1 y false a 0, podemos contar quienes han aprobado

notes[notes >= 5] #'Filtra las notas de los aprobados
notes[notes<5] #'Filtra las notas de los suspensos
notes[(notes > 9) | (notes < 1)] # existe && y  || pero no funciona con vectores (con vectores solo 1)

#' Bucles for:
#' La variable i va cogiendo el valor del 1 al 10
for(i in 1:10){
  print(i)
}



#Funciones

my_sum = function(x,y = 1){
  #es lo mismo que poner return(x+y)
  x + y
}

print(my_sum(3,4))
#se puede poner un valor por defecto que si no pones nada s utilizara
print(my_sum(3))


#######################
#Intento de hacer una grafica mundial de los casos
time = unique(newCovid$time)

cases = 0
for(i in 1:length(countries)){
  for(country in countries){
    pais = newCovid[newCovid$country == country, ]
    if(sum(newCovid[pais$time == time[i],]) != 0){
      cases = cases + pais$cases
    }
  }
  if(i == 1){
    plot(time[i], cases, type = h)
  }else{
    points(time[i], cases, type = h)
  }
}
##################################

#Apuntes de gráficas:


#' Para crear una gráfica se utiliza la funcion plot()
#' Sus parametros son:
#' eje x, eje y, tipo de gráfica (type), texto eje X (xlab), texto eje Y (ylab),
#' titulo de la gráfica (main), limite para eje Y (ylim), limite para eje X (xlim), etc.
plot(pais$time, pais$cum_rate, type = "l",
     xlab = "Time (day sice 1/1/2019)",
     ylab = "Cumulative inf. rate",
     main = "Covid 19 pandemic",
     ylim = c(0, 250)
)

#' Para añadir información a la gráfica utilizamos la función points()
#' Sus parametros son:
#' eje X, eje Y, color (col), tipo de gráfica(type), etc.
points(pais$time, pais$cum_rate, col = "black", type = "l")

#' Para añadir una leyenda utilizamos la función legend()
#' Sus parametros son:
#' Donde va a estar situado (ej: "toprigth"), paises u objetos tratados en el gáfico,
#' colores (col), que se vea una linea (lty).
legend("topleft", legend = countries, col = 1:length(countries), lty = 1)



#########################
demo = function(x){
  x = 1
  x
}

#Java: paso por referencia: lo que cambia dentro, cambia fuera
#R: paso por copia: lo que cambia dentro, NO cambia fuera.
x = 5
demo(x)
print(x)



#Programación funcional todo se hace con funciones
#funcion que coge un vector de elemento y una funcion y te va llamando a al funcion.
#Ya te pone el vector nombradp
resultado = sapply(countries, count_by_country, df = newCovid)

#Funciones lambda
resultado = sapply(
  countries,
  function(country){
    tmp = newCovid[newCovid$country == country,]
    sum(tmp$cases)
  }
)


#names() es para poner nombres a cada miembro
#Es como el colnames() de los dataframes, pero para vectores.
#Asumimos que la longitud de los vectores es la misma.
names(casos) = countries

print(casos)

print(casos[4])
print(casos["Andorra"])



#Como instalar un paquete o libreria
#Nunca deve de estar en el fichero. SE HACE EN EL TERMINAL
#VAmos a instalar cowsay
#install.packages("cowsay")

#Opción 1:
cowsay::say("hello world")

#Opción 2:
library("cowsay")
say("hello world")


