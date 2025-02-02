# Problema 1
matriz_hilbert <- function(n){
  matriz = matrix(nrow = n, ncol = n)
  for(i in 1:n){
    for(j in 1:n){
      matriz[i,j] = 1/(i+j-1)
    }
  }
  return(matriz)
}

print(matriz_hilbert(5))

for(i in 1:40){
  if (det(matriz_hilbert(i)) == 0) {
    print(i)
    break
  }
}
# Problema 2

simpson <- function(fun, a, b, n=10000) {
  h <- (b-a)/n
  x <- seq(a, b, by=h)
  if (n == 2) {
    s <- fun(x[1]) + 4*fun(x[2]) +fun(x[3])
  } else {
    s <- fun(x[1]) + fun(x[n+1]) + 2*sum(fun(x[seq(2,n-1,by=2)])) + 4 *sum(fun(x[seq(1,n, by=2)]))
  }
  s <- s*h/3
  return(s)
}

cuadrado <- function(x) x^2
seno <- function(x) sin(x)
ealamenos <- function(x) exp(-x**2)
raiz <- function(x) sqrt(1+x**3)
