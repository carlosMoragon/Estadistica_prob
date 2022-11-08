library(readr)

#' Section 1: Reading data, data types and indexing
#' =============================================================================
#' * Motivating problem: Download Covid-19 data by country and compare the cumulative
#' number of infections between two different countries.
#' * Topics: data types
#' * Section: "Beyond numbers and data types"-"Formulas" (included)
#' 


#' ### Exercise 1
#' Load data, and create a new data.frame with the following columns:
#' day, moth, year, cases, deaths, contriesAndTerritories, Cumulative...
#' Also, since the names are quite long, rename the latest two to 'country' and
#' 'cum_rate'. Tip: do not write all the names! Get the old ones and then override
#' just the latest two.
#' 

covid19 <- read_csv("covid19.csv")

newCovid= covid19[, c("day", "month", "year", "cases", "deaths", "countriesAndTerritories", "Cumulative_number_for_14_days_of_COVID.19_cases_per_100000")]

colnames(newCovid)[6:7] = c("country", "cum_rate")

#' ### Exercise 2
#' Since we want to plot number of cases against time we need a new column.
#' Why? Consider which variable should be used for the x-axis of the plot,
#' day?, month?, year?. Only using one of these variables will not work.
#' We must use these three variables to create a new variable named 'time' 
#' that approximately counts the number of days since the beginning of 2019. 
#' Use the formula: day + (month - 1) * 30 + (year - 2019) * 365
#' (there is an exact and simple way of transforming dates to numbers, but this
#' is not the point of the exercise). Then eliminate day, month and year from the
#' resulting data.frame
#' 

newCovid$time = newCovid$day + (newCovid$month - 1) * 30 + (newCovid$year - 2019) * 365
newCovid$day = NULL
newCovid = newCovid[, c(-1,-2)]


data = read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv",
                na.strings = "", fileEncoding = "UTF-8-BOM")

#' Section 2: logical indexing and plots
#' =============================================================================
#' * Motivating problem: Let's plot the 'cum_rate' variable against time for several
#' countries (Let's say Spain and Portugal), so we can compare their epidemiological 
#' situation
#' * Topics: logicals and logical indexing, plots
#' * Sections: Beyond numbers-formulas (both included), Simple graphics in R
#' 


#' ### Exercise 3 
#' plot the 'cum_rate' variable against 'time' for both Spain and Portugal. Proceed
#' as follows:
#' 1) create a data.frame with the data from Spain (name it 'spain') and then 
#' create another one from Portugal ('portugal').
#' 2) Use the plot function.
#' 

spain = newCovid[newCovid$country == "Spain", ]
portugal = newCovid[newCovid$country == "Portugal", ]
#type = "l" es una linea
#xlab es para el eje x
#ylab para el eje y
#main es el título
#ylim = para ajustar el limite del eje y
plot(spain$time, spain$cum_rate, type = "l",
     xlab = "Time (day sice 1/1/2019)",
     ylab = "Cumulative inf. rate",
     main = "Covid 19 pandemic",
     ylim = c(0, 500)
     )
points(portugal$time, portugal$cum_rate, col = "red", type = "l")
#lty es para poner una linea
legend("topleft", c("Spain", "Portugal"), col = c("black", "red"), lty = 1)
#View(newCovid)




#' Section 3: loops 
#' =============================================================================
#' * Motivating problem: Let's plot the 'cum_rate' variable against 'time' for a large
#' list of countries without duplicating code! 
#' situation
#' * Topics: loops
#' * Sections: Programming
#' 

#' ### Exercise 4 
#' Let's plot the 'cum_rate' variable against 'time' for 
#' the following countries: Spain, Portugal, Italy, France and Germany. You must
#' use a for/while loop!
#' 

countries = c("Spain","Portugal", "Italy", "France", "Germany")

#country es una variable que va tomando valores

i = 1
for(country in countries){
  
  pais = newCovid[newCovid$country == country, ]
  
  if(country == countries[1]){
    plot(spain$time, spain$cum_rate, type = "l",
         xlab = "Time (day sice 1/1/2019)",
         ylab = "Cumulative inf. rate",
         main = "Covid 19 pandemic",
         ylim = c(0, 250)
    )
  }else{
    points(pais$time, pais$cum_rate, col = i, type = "l")
  }
  i = i+1
}

legend("topleft", legend = countries, col = 1:length(countries), lty = 1)


#' Section 4: functions, packages and scoping
#' =============================================================================
#' * Motivating problem: Let's calculate the total number of cases since the 
#' beginning of data collection for the previous countries. This should be stored
#' in a new data.frame/vector. Then, let's print the information using some ASCII art.
#' * Topics: functions, scoping, packages, apply
#' * Sections: Functions, packages, scoping
#' 

#' ### Exercise 5 
#' Use a function and sapply to calculate the total number of cases since the 
#' beginning of data collection for the previous countries. This should be stored
#' in a new data.frame/vector. Then, print the information for each country,
#' using the say function from package 'cowsay' so that information is given 
#' by a shark (you must look at the documentation!)
#' 

count_by_country = function(df, country){
  #devuelve el numero de casos por pais
  #filtramos el numero de casos
  tmp = df[df$country == country,]
  sum(tmp$cases)
}

count_by_country(newCovid, "Germany")

#numero de casos de todos los paises
countries = unique(newCovid$country)
#countries = c("Spain","Portugal", "Italy", "France", "Germany")
casos = c()
for(pais in countries){
casos = c(casos,count_by_country(newCovid, pais))
}
#legend("topleft", legend = countries, col = 1:length(countries), lty = 1)
print(casos)

#names() es para poner nombres a cada miembro
#Es como el colnames() de los dataframes, pero para vectores.
#Asumimos que la longitud de los vectores es la misma.
names(casos) = countries

print(casos)

print(casos[4])
print(casos["Andorra"])



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


#Como instalar un paquete o libreria
#Nunca deve de estar en el fichero. SE HACE EN EL TERMINAL
#VAmos a instalar cowsay
#install.packages("cowsay")

#Opción 1:
cowsay::say("hello world")

#Opción 2:
library("cowsay")
say("hello world")

#instalar:
#install.packages("tidyverse")
