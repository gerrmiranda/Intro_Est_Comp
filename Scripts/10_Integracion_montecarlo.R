#ejemplo 7.1 de Byron Morgan



#hit and miss

n<-10000

set.seed(1234)

U1<-runif(n)

U2<-runif(n)

T<-U1**2+U2**2

T<-T<1

table(T)

head(cbind(U1,U2,T))

plot(U1,U2,col=T+1)





R<-sum(T)

(epsilon1<-pi-(4*R)/n)

(epsilon2<-epsilon1/pi)



#crude Monte Carlo

#creamos vector de Ui

set.seed(1234)

n<-10000

X<-runif(n)

Y<-1-X**2

Z<-sqrt(Y)

head(cbind(X,Y,Z))



plot(X,Y)

(epsilon2<-(sum(Z)/n)*4-pi)



#visto como B(n,pi/4)

ZZ<-rbinom(n,1,pi/4)


a <-2 ; b <-4 ; c <-0 ; d <-2

hx <- function (x){cos(2*x) -0.3*sin(x) +1}

(I<-integrate(hx,2,4))

x<-seq(a,b,0.01)

plot(x,hx(x),type="l")

abline(v=a,col=2)

abline(v=b,col=4)

abline(h=c,col=2)

abline(h=2.01,col=4)

# area del rectangulo

A <-(b-a)*(d-c)

A

# simulacion

n <- 1000
set.seed(1234)
u1 <- runif(n,a,b)
u2 <- runif(n,c,d)

# proporcion

T<-u2<hx(u1)
table(T)

p <- mean(u2<hx(u1))

# integral

A*p

plot(u1,u2,col=T+1)

lines(x,hx(x),type="l")



#Ejemplo de Jones 



f <- function(x) x^3 - 7*x^2 + 1

par(mfrow=c(1,1))

curve(f,0,1,xlim=c(-0.2,1.2),ylim=c(-7,3))

abline(v=0)

abline(v=1)

abline(h=-6)

abline(h=2)

hit_miss <- function(ftn, a, b, f.min, f.max, n) {
  
  # Monte-Carlo integration using the hit and miss method
  
  # ftn is a function of one variable
  
  # [a, b] is the range of integration
  
  # f.min and f.max are bounds on ftn over the range [a, b]
  
  # that is f.min <= ftn(x) <= f.max for all x in [a, b]
  
  # n is the number of samples used in the estimation
  
  # that is the number of calls made to the function ftn
  
  Z.sum <- 0
  
  for (i in 1:n) {
    
    X <- runif(1, a, b)
    
    Y <- runif(1, f.min, f.max)
    
    Z <- (ftn(X) >= Y)
    
    Z.sum <- Z.sum + Z
    
  }
  
  I <- (b - a)*f.min + (Z.sum/n)*(b - a)*(f.max - f.min)
  
  return(I)
  
}



hit_miss(hx, 2, 4, 0, 2, 5000)







hit_miss2 <- function(ftn, a, b, c, d, n) {
  
  # Monte-Carlo integration using the hit & miss method
  
  # partially vectorised version
  
  X <- runif(n, a, b)
  
  Y <- runif(n, c, d)
  
  Z <- (Y <= sapply(X, ftn))
  
  I <- (b - a)*c + (cumsum(Z)/(1:n))*(b - a)*(d - c)
  
  plot(1:n, I, type = "l")
  
  return(I[n])
  
}

hit_miss3 <- function(ftn, a, b, c, d, n) {
  
  # Monte-Carlo integration using the hit & miss method
  
  # partially vectorised version
  
  X <- runif(n, a, b)
  
  Y <- runif(n, c, d)
  
  Z <- (Y <= sapply(X, ftn))
  
  I <- (b - a)*c + (cumsum(Z)/(1:n))*(b - a)*(d - c)
  
  Int2=(b-a)*(d-c)*mean(Z)
  
  Sigma_Int=Int2*sqrt(mean(Z)*(1-mean(Z))/(n))
  
  plot(1:n, I, type = "l")
  
  return(Integral=list(hm=I,n=n,Int1=mean(I),Int2=Int2,Sigma_Int=Sigma_Int,LIIC=Int2-2*Sigma_Int,LSIC=Int2+2*Sigma_Int))
  
}



hit_miss2(hx, 2, 4, 0, 2, 1000)

lines(c(1, 10000), c(A*p, A*p))



n<-5000

integral_h_m<-hit_miss3(hx, 2, 4, 0, 2, n)

lines(c(1, n), c(integral_h_m$Int1, integral_h_m$Int1))

lines(c(1, n), c(integral_h_m$LIIC, integral_h_m$LIIC),col=2)

lines(c(1, n), c(integral_h_m$LSIC, integral_h_m$LSIC),col=4)

integral_h_m

names(integral_h_m)

summary(integral_h_m$hm)

sd(integral_h_m$hm)

plot(ecdf(integral_h_m$hm))





hit_miss2(f, 0, 1, -6, 2, 10000)

lines(c(1,10000 ), c(-13/12, -13/12))





mc_integral <- function(ftn, a, b, n) {
  
  # Monte Carlo integral of ftn over [a, b] using a sample of size n
  
  u <- runif(n, a, b)
  
  x <- sapply(u, ftn)
  
  return(mean(x)*(b-a))
  
}



mc_integral(hx,2,4,100000)











f<-function(x)return(1/(1+x))



mc_integral(f,0,1,1000)





mc_integral_a <- function(ftn, a, b, n) {
  
  # Integral Monte Carlo de ftn en [a, b] usando muestra de tamaño n pero variables antiteticas
  
  u1 <- runif(n, a, b)
  
  u2 <- 1-u1
  
  u<-c(u1,u2)
  
  x <- sapply(u, ftn)
  
  return(mean(x)*(b-a))
  
}





n<-1000

#Variables antiteticas

set.seed(1234)

U1<-runif(n)

U2<-1-U1

plot(U1,U2)

cor(U1,U2)







H=0.5*(sqrt(1-U1**2)+sqrt(1-(U2)**2))

plot(U1,H)

datos<-data.frame(U1,U2,H)

summary(datos)



(epsilon3<-pi/4-mean(datos$H))

apply(datos,2,sd)



par(mfrow=c(1,3))

y1<-function(x)return(sqrt(1-x**2))

curve(y1,0,1,col=1)

y2<-function(x)return(sqrt(1-(1-x)**2))

curve(y2,0,1,add=TRUE,col=2)



y3<-function(x)return(0.5*(sqrt(1-x**2)+sqrt(1-(1-x)**2)))

curve(y3,0,1,col=3,ylim=c(0,1))

y4<-function(x)return(0.25*(sqrt(1-x**2)+sqrt(1-(1-x)**2)+sqrt(1-(0.5-x)**2)+sqrt(1-(0.5+x)**2)))

curve(y4,0,0.5,col=4,ylim=c(0,1))





U3<-runif(n,0,0.5)



H2=0.25*(sqrt(1-U3**2)+sqrt(1-(1-U3)**2)+sqrt(1-(0.5-U3)**2)+sqrt(1-(0.5+U3)**2))



summary(H2)

datos<-cbind(datos,H2)

summary(datos)

(epsilon4<-pi/4-mean(datos$H2))



apply(datos,2,sd)





g <- function(x) 1 - x^2 # 

N <- 5000

# Numero de iteraciones

n <- 50

# Tamaño muestra

u_1 <- matrix(runif(2*n*N), ncol=N)

theta_1 <- colMeans(g(u_1))

u_a <- matrix(runif(n*N), ncol=N)

theta_a <- 0.5*(colMeans(g(u_a)) + colMeans(g(1 - u_a)))

var1 <- var(theta_1)

vara <- var(theta_a)

reduction <- 100*(var1 - vara)/var1

cat("Varianza theta_1 es", var1, "\n")



cat("Varianza theta_a es", vara, "\n")



cat("Reduccion Varianza es ", format(reduction, dig = 2), "porcent\n")







#Muestreo de importancia 

#Ejemplo de JOnes 





Ginv <- function(u){
  
  sqrt(2)*tan(u*atan(1/sqrt(2)))
  
}

Psi <- function(x){
  
  exp(-(x^2)/2)*sqrt(2)*atan(1/sqrt(2))*(1+(x^2)/2)
  
}

N <- 10000 # Number of estimates of each type

n <- 50

# Sample size

u_a <- matrix(runif(n*N), ncol=N)

theta_a <- colMeans(Psi(Ginv(u_a)))

var1 <- 1.413/n

vara <- var(theta_a)

reduction <- 100*(var1 - vara)/var1

cat("Variance theta_1 is", var1, "\n")

#Variance theta_1 is 0.02826

cat("Variance theta_a is", vara, "\n")

#Variance theta_a is 8.181764e-06

cat("Variance reduction is", format(reduction, dig = 2), "% \n")







#Control de Variables



Ginv <- function(u){
  
  sqrt(2) * tan(u * atan(1 / sqrt(2)))
  
}

psi1 <- function(x){
  
  exp(-(x^2)/2) * sqrt(2) * atan(1/sqrt(2)) * (1+(x^2)/2)
  
}

psi2 <- function(x){  
  
  (1-(x^2)/2) * sqrt(2) * atan(1/sqrt(2)) * (1+(x^2)/2)
  
}

N <- 10000 # Number of estimates of each type

n <- 50

# Sample size

commonG <- matrix(Ginv(runif(n*N)), ncol = N)

p1g <- psi1(commonG)

p2g <- psi2(commonG)

theta_hat <- colMeans(p1g)

mu_hat <- colMeans(p2g)

samplecov <- colSums((p1g - matrix(theta_hat, n, N, byrow = TRUE)) *
                       
                       (p2g - 5/6))/n

samplevar <- colSums((p2g - 5/6)^2)/n

alphastar <- samplecov/samplevar

theta_hat_c <- theta_hat - alphastar*(mu_hat - 5/6)

var1 <- var(theta_hat)

varc <- var(theta_hat_c)

reduction<-100*(var1-varc)/var1

cat("Variance theta_hat is", var1, "\n")

#Variance theta_hat is 8.487258e-06

cat("Variance theta_hat_c is", varc, "\n")

#Variance theta_hat_c is 9.136256e-08

cat("Variance reduction is", format(reduction, dig = 2), "percent\n")

#Variance reduction is 99 percent