# ******************************************************************************
# >>        Métodos no paramétricos de aleatoriedad
# ******************************************************************************
######  Prueba de rachas ####

#Por aproximación
library(randtests)

secuencia<-c("S","S", "S","S", "L", "S", "L", "S", "S", "L", "S", "S", "L", 
             "S", "S", "S", "L", "S", "S", "L", "L", "L", "S", "S", "S", "S",
             "L", "S", "L", "S", "L", "L", "L", "S", "L", "S", "S", "S", "L", 
             "S", "S", "S", "L", "S", "L", "S", "S", "L", "S", "S", "S", "L")

rachas<-ifelse(secuencia =="S",1,0) 

runs.test(rachas,alternative = "two.sided",threshold = 0.5,pvalue = "normal",plot=T)



# ******************************************************************************
# >>        Métodos no paramétricos para una muestra
# ******************************************************************************


######  Prueba del signo ####

library(BSDA)
datos = c(4,5,8,8,9,6,10,7,6,6)
SIGN.test(datos,md=5 ,alternative = "two.sided", conf.level = 0.95) 


# Otra manera de realizar la prueba
p.signo.bi <- function(muestra, H.null, a){
  #  "+" Mayor a la H. nula
  #  "-" Menor a la H. nula
  #  "0" igual a la H. nula
  signos = c()
  for (valor in datos){
    if (valor > H.null){
      
      signos <- c(signos, '+')
      
    }else if (valor < H.null){
      
      signos <- c(signos, '-')
      
    }else{
      signos <- c(signos, '0')
    }
  }
  # Frecuencia de signos
  frecuencia <- table(signos)
  k = frecuencia[["-"]]
  n = frecuencia[["-"]] + frecuencia[["+"]]
  
  # Calcular P-valor
  p.value = pbinom(k, n, 0.5) * 2
  
  print(paste("Número de signos negativos:", frecuencia[["-"]]))
  print(paste("Número de signos positivos:", frecuencia[["+"]]))
  print(paste("P-valor:", p.value))
  print(paste("Mediana:", median(muestra)))
  if (p.value < a){
    print(paste("Se rechaza la hipótesis nula y se decide por la alternativa, la mediana es diferente a", H.null))
    
  }else{
    print(paste("No hay suficiente evidencia estadística para indicar que la mediana no es", H.null))
  }
}

# Muestra
datos = c(4,5,8,8,9,6,10,7,6,6)

# Hipótesis
H.null = 5

p.signo.bi(muestra = datos, H.null = H.null, a = 0.05)


###### Prueba de rango con signo de Wilcoxon ####
muestra<-c(99, 100, 90, 94, 135, 108, 107, 111, 119, 104, 127, 109, 117, 105, 125)

#H0: M=107
wilcox.test(muestra,mu=107,exact=F,correct = T, alternative =  "two.sided", conf.int = 0.95)


# ******************************************************************************
# >>        Métodos no paramétricos para 2 muestras independientes
# ******************************************************************************

###### Prueba de la mediana ####

#Lectura de datos
datos <- data.frame(
  Preparatoria= c(rep("Urbana", 16), rep("Rural", 12)),
  Calificacion = c(35,25,26,27,27,45,21,46,27,33,38,26,23,46,25,41,29,50,50,37,43,34,22,31,42,47,42,32))

#Aplicación de la prueba
library(RVAideMemoire)
mood.medtest(datos$Calificacion,datos$Preparatoria,exact = TRUE)


# Otra manera de  realizar la prueba
p.signo.d.m <- function(muestra, a){
  #  "+" Xi - Yi > 0
  #  "-" Xi - Yi < 0
  #  "0" Xi - Yi = 0
  
  signos = c()
  tam <- length(muestra$NAA)
  for (i in c(1:tam)){
    if (muestra$NAA[[i]] < muestra$NAD[[i]]){
      signos = c(signos, "-")
      
    }else if (muestra$NAA[[i]] > muestra$NAD[[i]]){
      signos = c(signos, "+")
      
    }else{
      signos = c(signos, "0")
    }
    
  }
  
  
  frecuencia <- table(signos)
  k = frecuencia[["-"]]
  n = frecuencia[["-"]] + frecuencia[["+"]]
  # Calcular P-valor
  p.value = pbinom(k, n, 0.5)
  print(paste("Decisión estadística con α=", a))
  print(paste("P-valor:", p.value))
  print("")
  if (p.value < a){
    print(paste("P-valor:", p.value, "< α:",a ,". Se rechaza H0 y se decide por H1."))
  }else{
    print(paste("P-valor:",p.value, "> α:",a ,". Se acepta H0."))
  }
  print("")
  print(paste("Diferencias negativas:", k))
  print(paste("Diferencias positivas:", frecuencia[["+"]]))
  print(paste("Empates:", n - tam))
  print(paste("Total:", n + n - tam))
}


d.datos <- data.frame(
  "NAA" = c(3,5,2,3,3,3,0,4,1,6,4,1), 
  "NAD" = c(1,2,0,2,2,0,2,3,3,4,1,0)
)
p.signo.d.m(d.datos, 0.05)


# ******************************************************************************
# >>        Métodos no paramétricos para más de 2 muestras independientes
# ******************************************************************************


###### Kruskal Wallis ####

datos <- data.frame(
  ciudad= c(rep("Ciudad 1", 5), rep("Ciudad 2", 5), rep("Ciudad 3", 5)),
  Ventas = c(10,14,18,15,12,16,18,22,18,15,15,12,8,10,13))

head(datos)

library(ggplot2)
ggplot(data = datos, mapping = aes(x =ciudad, y = Ventas, colour = ciudad)) +
  geom_boxplot() +
  theme_bw() +
  theme(legend.position = "none",plot.title = element_text(hjust = 0.5)) +
  labs(title="Gráfica de caja y bigotes")

aggregate(Ventas ~ ciudad, data = datos, FUN = median)

kruskal.test(Ventas ~ ciudad, data = datos)


# ******************************************************************************
# >>        Métodos no paramétricos para 2 muestras relacionadas
# ******************************************************************************

#Lectura de datos
antes<-c(3,5,2,3,3,3,0,4,1,6,4,1)
despues<-c(1,2,0,2,2,0,2,3,3,4,1,0)

#Aplicación de la muestra
library(BSDA)
SIGN.test(antes,despues,alternative = "greater",conf.level = 0.95)


# ******************************************************************************
# >>        Métodos no paramétricos para más de 2 muestras relacionadas
# ******************************************************************************

###### Friedman ####

Sujeto <- factor( rep( 1:5, each = 3 ) )
Turno<- factor( rep( c( "mañana", "tarde", "noche" ), 5 ) )
Productos<- c( 31, 25, 35,
              33, 26, 33,
              28, 24, 30,
              30, 29, 28,
              28, 26, 27)


datos <- data.frame( Sujeto, Turno, Productos )
head(datos,6)

library(ggplot2)
ggplot(data = datos, mapping = aes(x = Turno, y = Productos, colour = Turno)) +
  geom_boxplot() +
  theme_bw() +
  theme(legend.position = "none",plot.title = element_text(hjust = 0.5)) +
  labs(title="Gráfica de caja y bigotes")

by(data = datos$Productos, INDICES = datos$Turno, FUN = median)

friedman.test(Productos ,Turno, Sujeto)




