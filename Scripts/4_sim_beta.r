M <- 1.5 # valor de la constante de normalizacion
# valores para comenzar el while
U <- Inf
Y <- 0.5
# contador de simulaciones
j <-0
while (U > dbeta (Y,2,2)/(M*dunif(Y,0,1))) {
Y <- runif(1)
U <- runif(1)
j <-j +1
print(c(j,U))
}


sim_beta<-function(n,alfa,beta) {
xM<-(alfa-1)/(alfa + beta -2) # valor de la constante de normalizacion
M <- dbeta( xM , alfa , beta )
x <- rep(0 , n )
# vector para almacenar las simulaciones
J <-0
# contador para registrar el N total de simulaciones
for ( i in 1: n ) {
# valores para comenzar el while
U <- Inf
Y <- 0.5
# contador de simulaciones
j <-0
while (U > dbeta(Y , alfa , beta ) / ( M * dunif(Y ,0 ,1) ) ) {
Y <- runif(1)
U <- runif(1)
j <-j +1
}
x[i]<-Y
J <-J + j
}
print(J)
return( x )
}


sim_beta(n=1000,2,2)
