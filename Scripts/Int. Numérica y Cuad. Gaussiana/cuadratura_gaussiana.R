# Cuadratura Guassiana

# Primer ejemplo int_0^4 texp(2t)dt

# funcion a integrar
fx<-function(x)(x*exp(2*x))
curve(fx,0,1)
gx<-function(x)(4*x+4)*exp(4*x+4)
curve(gx,0,1)
I<-integrate(fx,0,4)
I
# libreria que nos permite obtener nodos y pesos para
# distintos esquemas de guadratura (Legendre, Hermite, Laguerre, etc)
library(statmod)

?gauss.quad

#Gaussian Quadrature
#Description
#Calculate nodes and weights for Gaussian quadrature.

out <- gauss.quad(2,'legendre')
out
cuadratura<-function(FUN,n=2,cual='legendre'){
  xw<-gauss.quad(n,kind=cual)
  nodos<-xw$nodes
  pesos<-xw$weights
  Int<-sum(pesos*FUN(nodos))
  invisible(Int)
}

I2<-cuadratura(gx,2)
I2
I3<-cuadratura(gx,3)
I3
I4<-cuadratura(gx,4)
I4

# Calculo del error relativo
# ????
error<-matrix(0,3,4,dimnames = list(seq(2,4,1),c("n", "I","EA","ER")))
error[,1]<-seq(2,4,1)
error[,2]<-c(I2,I3,I4)
error[,3]<-error[,2]-I$value
error[,4]<-error[,3]/I$value
error
# ?Cuantos nodos se necesitan?
tol<-1e-5
erel<-Inf

# estimacion inicial (n=2)
Int0<-cuadratura(gx,2)
n<-3

while (erel>tol){

  # calculo de la integral
  Int<-cuadratura(gx,n)
  
  # calculo del error relativo
  erel<-abs(Int-Int0)/Int0
  
  cat('El valor de la integral es',round(Int,4),'\n')
  
  # se incrementa el valor de n
  n<-n+1
  Int0<-Int
}

#Veamos los pesos y nodos de Gauss Legendre
list(gauss.quad(1,'legendre'),gauss.quad(2,'legendre'),gauss.quad(3,'legendre'))

#Los nodos son las raíces de los polinomios de Legrendre

library(orthopolynom)

legendre.recurrences(5,normalized=FALSE)
lista.PLN<-legendre.polynomials(5, normalized=FALSE)
lista.PL<-legendre.polynomials(5, normalized=TRUE)
print(lista.PL)
print(lista.PLN)
plot(lista.PLN[[1]])

for (i in 1:5){
  plot(lista.PLN[[i]],new=TRUE)
}

for (i in 1:5){
  plot(lista.PL[[i]],new=TRUE)
}

for (i in 1:5){
  par(mfrow=c(1,2))
  plot(lista.PL[[i]],new=TRUE)
  plot(lista.PLN[[i]],new=TRUE)
}

lista.PL[[3]]
hx<-as.function(lista.PL[[3]])
hx(0)
par(mfrow=c(1,1))
plot(lista.PL[[3]],new=TRUE)
points(gauss.quad(2,'legendre')[[1]][2],hx(gauss.quad(2,'legendre')[[1]][2]),col=4)
points(gauss.quad(2,'legendre')[[1]][1],hx(gauss.quad(2,'legendre')[[1]][1]),col=2)


plot(lista.PLN[[3]],new=TRUE)
points(gauss.quad(2,'legendre')[[1]][2],hx(gauss.quad(2,'legendre')[[1]][2]),col=4)
points(gauss.quad(2,'legendre')[[1]][1],hx(gauss.quad(2,'legendre')[[1]][1]),col=2)


hx(gauss.quad(2,'legendre')[[1]][1])

?legendre.inner.products
h <- legendre.inner.products(5)
print(h)

# Calculando
#  el cuarto momento de la normal estándar es 3
out <- gauss.quad.prob(10,"normal")
out
sum(out$weights * out$nodes^4)

#  the expected value of log(X) where X is gamma is digamma(alpha)
out <- gauss.quad.prob(32,"gamma",alpha=5)
sum(out$weights * log(out$nodes))

library(gaussquad)

###
### este ejemplo evalúa la función de cuadratura para
### los polinomios de Legendre. 
### calcula la integral del producto para todos los pares de polinomios ortogonales del pedido 0 al pedido n. 
### los resultados se comparan con la matriz diagonal de los productos internos para el polinomios. también calcula la integral del producto de todos los pares de polinomios ortonormales de orden 0 a orden n. 
### la matriz resultante debe ser una matriz identidad
###
### establece el valor para el orden polinomial máximo
###
n <- 5
###
### maximo orden + 1
###
np1 <- n + 1
### construye una lista de los polinomios ortogonales de Legendre
###
p.list <- legendre.polynomials(n)
p.list
lista.PLN
###
### obtiene la tabla de reglas, es decir nodos y pesos  para el polinomio de orden np1
###
rules <- legendre.quadrature.rules(np1)
rules
order.np1.rule <- rules[[np1]]
order.np1.rule
### construye la matriz diagonal con los productos internos
### de los polinomios ortogonales en la diagonal
###
?legendre.inner.products(n)
legendre.inner.products(n)
p.p.inner.products <- diag(legendre.inner.products(n))
print( apply( p.p.inner.products, 2, round, digits=5 ) )





# Cambio de variable para integrales indefinidas
# se quiere integrar I = int_{-oo}^{oo} 1/sqrt(2*pi)*exp(0.5*t^2)

# luego del cambio de variables, el integrando es:
integrando<-function(x) dnorm(x/(1-x^2),0,1)*(1+x^2)/(1-x^2)^2

for (n in 2:50){
  In<-cuadratura(integrando,n)
  cat('Con ',n,' nodos el valor de la integral es',round(In,4),'\n')
}



# Segundo ejemplo (Gauss-Hermite)
# E(cos(t)) con t~N(0,1)

ht<-function(t) cos(sqrt(2)*t)
Ecost <- cuadratura(ht,n=2,cual='hermite')

# 2 nodos es suficiente?
tol<-1e-5
erel<-Inf
# estimacion inicial (n=2)
Int0<-Ecost
n<-3

while (erel>tol){

  # calculo de la integral
  Int<-cuadratura(ht,n,cual='hermite')/sqrt(pi)
  
  # calculo del error relativo
  erel<-abs(Int-Int0)/Int0
  
  cat('E(cos(t))',round(Int,4),'\n')
  
  # se incrementa el valor de n
  n<-n+1
  Int0<-Int
}

# compare el resultado anterior con el siguiente
set.seed(1234)
N<-100000
x<-rnorm(N)
mean( cos(x) )



# Ultimo ejemplo (Gauss-Laguerre)
# E(log(t)) con t~Exp(lambda)

lambda<-1
ht<-function(t) log(t/lambda)
Elogt <- cuadratura(ht,n=2,cual='laguerre')

# 2 nodos es suficiente?
tol<-1e-5
erel<-Inf
# estimacion inicial (n=2)
Int0<-Elogt
n<-3

while (abs(erel)>tol){

  # calculo de la integral
  Int<-cuadratura(ht,n,cual='laguerre')
  
  # calculo del error relativo
  erel<-abs(Int-Int0)/Int0
  
  cat('E(log(t))',round(Int,4),'\n')
  
  # se incrementa el valor de n
  n<-n+1
  Int0<-Int
}

# compare el resultado anterior con el siguiente
set.seed(1234)
N<-100000
x<-rexp(N,lambda)
mean( log(x) )




library(gaussquad)

###
### este ejemplo evalúa la función de cuadratura para
### los polinomios de Legendre. 
### calcula la integral del producto para todos los pares de polinomios ortogonales del pedido 0 al pedido n. 
### los resultados se comparan con la matriz diagonal de los productos internos para el polinomios. también calcula la integral del producto de todos los pares de polinomios ortonormales de orden 0 a orden n. 
### la matriz resultante debe ser una matriz identidad
###
### establece el valor para el orden polinomial máximo
###
n <- 5
###
### maximo orden + 1
###
np1 <- n + 1
###
### función para construir los productos de polinomios por columna
###
by.column.products <- function( c, p.list, p.p.list )
{
  ###
  ### función para construir los productos de polinomios por fila
  ###
  by.row.products <- function( r, c, p.list )
  {
    row.column.product <- p.list[[r]] * p.list[[c]]
    return (row.column.product )
  }
  np1 <- length( p.list )
  row.list <- lapply( 1:np1, by.row.products, c, p.list )
  return( row.list )
}
###
### función construir las funciones polinómicas por columna
###
by.column.functions <- function( c, p.p.products )
{
  ###
  ### función para construir las funciones polinómicas por fila
  ###
  by.row.functions <- function( r, c, p.p.products )
  {
    row.column.function <- as.function( p.p.products[[r]][[c]] )
    return( row.column.function )
  }
  np1 <- length( p.p.products[[1]] )
  row.list <- lapply( 1:np1, by.row.functions, c, p.p.products )
  return( row.list )
}
###
### función para calcular la integral de los polinomios por columna
###
by.column.integrals <- function( c, p.p.functions )
{
  ###
  ### function to compute the integral of the polynomials by row
  ###
  by.row.integrals <- function( r, c, p.p.functions )
  {
    row.column.integral <- legendre.quadrature(
      p.p.functions[[r]][[c]], order.np1.rule )
    return( row.column.integral )
  }
  np1 <- length(p.p.functions[[1]])
  row.vector<-sapply(1:np1, by.row.integrals, c, p.p.functions )
  return( row.vector )
}
###
### construye una lista de los polinomios ortogonales de Legendre
###
p.list <- legendre.polynomials(n)
p.list
lista.PLN
###
### construye la lista bidimensional de productos de pares
### de polinomios
###
p.p.products <- lapply( 1:np1, by.column.products, p.list )
str(p.p.products)
###
### calcula la lista bidimensional de funciones
### correspondiente a los productos polinómicos en
### la lista bidimensional p.p.products
###
p.p.functions <- lapply( 1:np1, by.column.functions, p.p.products )
p.p.functions
###
### obtiene la tabla de reglas, es decir nodos y pesos  para el polinomio de orden np1
###
rules <- legendre.quadrature.rules(np1)
rules
order.np1.rule <- rules[[np1]]
order.np1.rule
###
### construye la matriz cuadrada simétrica que contiene
### las integrales definidas sobre los límites predeterminados
### correspondiente a la lista bidimensional de
### funciones polinómicas
###
p.p.integrals <- sapply( 1:np1, by.column.integrals, p.p.functions )
p.p.integrals
###
### construye la matriz diagonal con los productos internos
### de los polinomios ortogonales en la diagonal
###
?legendre.inner.products(n)
legendre.inner.products(n)
p.p.inner.products <- diag(legendre.inner.products(n))
print( "Integral de productos cruzados para  los polinomios ortogonales " )
print( apply( p.p.integrals, 2, round, digits=5 ) )
print( apply( p.p.inner.products, 2, round, digits=5 ) )