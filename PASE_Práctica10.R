### Práctica 10
#Ejercicio I
library(MASS) #Agregamos algunas librarías que nos ayuden a graficar
library(ggplot2) #Con el estilo de un heatmap
library(viridis)
library(scatterplot3d)

get_density <- function(x, y, ...) { #Función que determina la densidad
  dens <- MASS::kde2d(x, y, ...) #De los puntos; no importante
  ix <- findInterval(x, dens$x) #Para la práctica
  iy <- findInterval(y, dens$y)
  ii <- cbind(ix, iy)
  return(dens$z[ii])
}

#Ejercicio 1
Ejer1 <- function(n){
  set.seed(1234)
  xs <- rnorm(n)
  ys <- rnorm(n)
  #Necesitamos un dataframe para la función que grafica de acuerdo a la densidad
  dat <- data.frame(x = xs,y = ys)
  dat$density <- get_density(dat$x, dat$y, n = 100) #Sacamos las densidades
  gg0 <- ggplot(dat) + geom_point(aes(xs, ys, color = density)) + #Graficamos con densidad
    scale_color_viridis() + ggtitle("X vs Y") +
    labs(y="y", x = "x")

  dat1 <- data.frame(x = xs+ys,y = xs-ys)
  dat1$density <- get_density(dat1$x, dat1$y, n = 100)
  gg1 <- ggplot(dat1) + geom_point(aes(xs+ys, xs-ys, color = density)) +
    scale_color_viridis() + ggtitle("X+Y vs X-Y")+
    labs(y="x-y", x = "x+y")
  show(gg0)
  show(gg1)
}
Ejer1(1000)

#Ejercicio2

Multinomial2 <- function(m,n,p1,p2,p3){
  set.seed(1)
  vec <- c()
  for (k in 1:m){
    a <- runif(n)
    probas <- c(p1,p2,p3)
    multi <- c(0,0,0)
    for (i in 1:n){
      start <- 0
      azar <- a[i]
      for(j in 1:3){
        end <- start + probas[j]
        if(azar<=end && azar>start){
          multi[j] <- multi[j]+1
        }
        start <- end
      }
    }
    vec <- cbind(vec,multi)
  }
  return(vec)
}

#Parametros utilizados

Ejercicio2 <- function(n){
  m <- t(Multinomial2(400,n,0.25,0.25,0.5))
  plotting <-
    scatterplot3d(m[,1:3], angle = 135, pch = 16, color = "steelblue",
                  #type = 'h',
                  main = 'Multinomial',
                  sub = paste("n=",n),
                  xlab = "X1",
                  ylab = "X2",
                  zlab = "X3")
  show(plotting)
}

Ejercicio2(1000)


#Ejercicio 3

Ejercicio3 <- function(m,n,p1,p2,p3){
  set.seed(1)
  vec <- c()
  for (k in 1:m){
    a <- runif(n)
    probas <- c(p1,p2,p3)
    multi <- c(0,0)
    for (i in 1:n){
      start <- 0
      azar <- a[i]
      for(j in 1:2){
        end <- start + probas[j]
        if(azar<=end && azar>start){
          multi[j] <- multi[j]+1
        }
        start <- end
      }
    }
    vec <- cbind(vec,multi)
    vec1 <- data.frame(t(vec))
  }
  vec1$density <- get_density(vec1$X1, vec1$X2, n = 100)
  gg2 <- ggplot(vec1) + geom_point(aes(X1, X2, color = density)) +
    scale_color_viridis() + ggtitle(paste("Multinomial con n=",n,"m=",m,"p1=",p1,"p2=",p2,"p3=",p3))+
    labs(y="x-y", x = "x+y")
  show(gg2)
}

Ejercicio3(1000,50,0.16,0.16,0.66)
Ejercicio3(1000,500,0.16,0.16,0.66)
Ejercicio3(1000,1000,0.16,0.16,0.66)
