#------------------------------------------------------------------------------#
#           Metodos Econometricos y Organizacion Industrial Aplicada
#                    Trabajo Practico 2 - Ejercicio 1 
#                       Estudiante:  Paola Llamas
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
# Set up 
setwd("/Users/paollamas/Desktop/TP2 OI/Ejercicio 1") # Seteamos el directorio 

# Si no tenemos los paquetes instalados los instalamos
#install.packages("ggplot2")
#install.packages("extrafont")
#install.packages("KernSmooth")

# Cargamos los paquetes necesarios
library(ggplot2)
library(extrafont)
library(KernSmooth)

# Importamos la base y creamos elementos preliminares
data_matrix <-read.csv("FPAData.csv")
data <- as.data.frame(data_matrix)
set.seed(80) # seteamos semilla por las dudas
colnames(data) <- c("Auction", "B1", "B2") # modificaciones columnas de la base
all_bidds <- c(data$B1, data$B2) # armamos un vector que contenga todas las ofertas
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# PRIMERA ETAPA GPV 
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# Paso 1 :  Estimación distribucion de las ofertas
#------------------------------------------------------------------------------#
ecdf(all_bidds)
G_b <- ecdf(all_bidds)
plot(G_b, xlim= c(4.5,10.5), main = "", ylab = "Probabilidad acumulada", 
     xlab = "Ofertas", col = "blue", lwd=1.5) # graficamos

# Podemos hacer G_b(x) para ver cuanto vale la ECDF en el punto x. Esto nos va a 
# servir para construir luego las pseudo valuaciones. 
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# Paso 2: Estimación de la densidad de las ofertas, vamos a usar kernel "Biweight" 
#------------------------------------------------------------------------------#

# Para el bandwidth utilizamos la regla de Silverman, como detallan los autores en
# el paper
sigmab <- sd(all_bidds)
cvk <- 2.78 #constante de Kernel biweight con v = 2
IL <- 1000
hg <- (sigmab*cvk)*((IL)^(-1/5)) # bandwidth optimo
hg

# ahora estimamos la densidad como ya hicimos en trabajo anterior
d1 <- density(all_bidds, kernel = "biweight", bw = hg)
plot(d1, lwd = 3, main = "", col = "blue") # graficamos 
density_at_specific_points <- approx(d1$x, d1$y, xout = all_bidds) 
# este codigo de arriba lo que hace es nos dice para cada valor de las ofertas, 
# cual es la densidad estimada en esos puntos. Esto nos va a ser especialmente util
# cuando definamos las pseudo valuaciones 
density_estimates_of_bids  <- density_at_specific_points[["y"]] 
# guardamos el valor de todas las densidades en este vector
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# Paso 3: Construir pseudo valuaciones
#------------------------------------------------------------------------------#

# Lo que hacemos primero es construir un diccionario que contenga pares (x,y) 
# donde x es una oferta e y es la densidad estimada de la oferta correspondiente 

my_dictionary <- list()
for (i in 1:length(all_bidds)) {
  my_dictionary[[as.character(all_bidds[i])]] <- density_estimates_of_bids[i]
}

# definimos otros objetos que nos van a servir para encontrar las 
# pseudo valuaciones
pseudo_v <-  c() # creamos este vector vacio, aca vamos a ir poniendo las pseudov
min_bid <- min(all_bidds) # calculamos el minimo de las ofertas
max_bid <- max(all_bidds) # calculamos maximo de ofertas 
p_g <- 2 
# esto es la longitud del soporte del kernel que estamos usando (=2). Lo usan GPV
# en el paper

# ahora iteramos: lo que hace la sentencia condicional es agarra un par 
# (oferta, densidad(oferta)). 
# Luego se fija si la oferta seleccionada cumple cierta condicion, 
# es decir si esta dentro de cierto rango de su soporte estimado. 
# si cumple con la condicion, definimos la valuacion asociada como hacen GPV 
# (la formula esta en el paper y la cite en el trabajo)
# si la oferta seleccionada no cumple con la condicion, entonces le asociamos un valor 
# infinito a la valoracion, ya que esta sera luego desechada. 

for (key in names(my_dictionary)) {
  bid <- as.numeric(key)  # Convert the key (bid) back to numeric type # estas son las ofertas
  density_estimate_of_bid <- my_dictionary[[key]]  # select density for the corresponding bid
  
  if ( ( bid >= min_bid + p_g*hg/2) &  (bid <= max_bid - p_g*hg/2) ) 
    {
    new_valuation <- bid + ((1/(2-1))*(G_b(bid)/density_estimate_of_bid)) } 
  else {
      new_valuation <- Inf 
      }
   pseudo_v <- c(pseudo_v, new_valuation) 
}

pseudo_final <- pseudo_v[pseudo_v < Inf] 
# eliminamos todas las valoraciones estimadas que dieron infinito. O sea, 
# estamos eliminando todas las valoraciones cuya oferta correspondiente no se
# encontro dentro del soporte seleccionado. 
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# SEGUNDA ETAPA GPV 
#------------------------------------------------------------------------------#

# Ahora que ya tenemos las pseudo valuaciones, lo unico que resta es estimar la
# densidad (y esto ya sabemos como hacerlo!)
# Lo unico que hay que tener en cuenta es que hay que usar el optimal GPV bandwidth 
# para que el estimador alcance la tasa de convergencia optima que proponen 
# los autores

# optimal bandwidth GPV for valuation density: Silverman!
sigmav <-  sd(pseudo_final)
ILt  <-  length(pseudo_final)
hf <- (sigmav*cvk)*((ILt)^(-1/5))
hf

d2 <- density(pseudo_final, kernel = "biweight", bw = hf) 
plot(d2, lwd = 3, main = "", col = "blue") # graficamos
x_range <- range(d1$x, d2$x)
axis(1, at = seq(ceiling(x_range[1]), floor(x_range[2])), labels = seq(ceiling(x_range[1]), floor(x_range[2])))
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# Graficos adicionales
#------------------------------------------------------------------------------#

plot(d1, main="",xlim = c(2,18),  ylim = c(0,0.20), xlab="Support", ylab="Density", col="blue", lwd=3)
# Add the second density curve to the existing plot
lines(d2, col="red", lwd=3)
x_range <- range(d1$x, d2$x)
axis(1, at = seq(ceiling(x_range[1]), floor(x_range[2])), labels = seq(ceiling(x_range[1]), floor(x_range[2])))
legend("topright", legend=c("Bids Density", "Values Density"), col=c("blue", "red"), lwd=2)
#------------------------------------------------------------------------------#


#------------------------------------------------------------------------------#
# # Adicional: Comparamos densidad de las valuaciones cambiando el Kernel 
#------------------------------------------------------------------------------#
# Para el bandwidth utilizamos la regla de Silverman, como detallan los autores en
# el paper
sigmab <- sd(all_bidds)
cvk <- 2.34 #constante de Kernel Epanechnikov con v = 2
IL <- 1000
hg <- (sigmab*cvk)*((IL)^(-1/5)) # bandwidth optimo
hg

d1E <- density(all_bidds, kernel = "epanechnikov", bw = hg)
density_at_specific_points_E <- approx(d1E$x, d1E$y, xout = all_bidds) 
density_estimates_of_bids_E  <- density_at_specific_points_E[["y"]] 
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# Paso 3: Construir pseudo valuaciones
#------------------------------------------------------------------------------#

my_dictionary <- list()
for (i in 1:length(all_bidds)) {
  my_dictionary[[as.character(all_bidds[i])]] <- density_estimates_of_bids_E[i]
}

# definimos otros objetos que nos van a servir para encontrar las 
# pseudo valuaciones
pseudo_v <-  c() # creamos este vector vacio, aca vamos a ir poniendo las pseudov
min_bid <- min(all_bidds) # calculamos el minimo de las ofertas
max_bid <- max(all_bidds) # calculamos maximo de ofertas 
p_g <- 2 

for (key in names(my_dictionary)) {
  bid <- as.numeric(key)  # Convert the key (bid) back to numeric type # estas son las ofertas
  density_estimate_of_bid <- my_dictionary[[key]]  # select density for the corresponding bid
  
  if ( ( bid >= min_bid + p_g*hg/2) &  (bid <= max_bid - p_g*hg/2) ) 
  {
    new_valuation <- bid + ((1/(2-1))*(G_b(bid)/density_estimate_of_bid)) } 
  else {
    new_valuation <- Inf 
  }
  pseudo_v <- c(pseudo_v, new_valuation) 
}

pseudo_final <- pseudo_v[pseudo_v < Inf] 

#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# SEGUNDA ETAPA GPV 
#------------------------------------------------------------------------------#

# optimal bandwidth GPV for valuation density: Silverman!
sigmav <-  sd(pseudo_final)
ILt  <-  length(pseudo_final)
hf <- (sigmav*cvk)*((ILt)^(-1/5))
hf
d2E <- density(pseudo_final, kernel = "biweight", bw = hf) 

# Plot Final 
plot(d2, main="",xlim = c(2,18),  ylim = c(0,0.20), xlab="Support", ylab="Density", col="blue", lwd=3)
# Add the second density curve to the existing plot
lines(d2E, col="red", lwd=3)
x_range <- range(d1$x, d2$x)
axis(1, at = seq(ceiling(x_range[1]), floor(x_range[2])), labels = seq(ceiling(x_range[1]), floor(x_range[2])))
legend("topright", legend=c("Biweight", "Epanechnikov"), col=c("blue", "red"), lwd=2)
#------------------------------------------------------------------------------#




















