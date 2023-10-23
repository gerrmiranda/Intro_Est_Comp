#program spuRs/resources/scripts/simpson_n.r

simpson_n <- function(ftn, a, b, n = 100) {
  # numerical integral of ftn from a to b
  # using Simpson's rule with n subdivisions
  #
  # ftn is a function of a single variable
  # we assume a < b and n is a positive even integer

  n <- max(c(2*(n %/% 2), 4))
  h <- (b-a)/n
  x.vec1 <- seq(a+h, b-h, by = 2*h)
  x.vec2 <- seq(a+2*h, b-2*h, by = 2*h)
  f.vec1 <- sapply(x.vec1, ftn)
  f.vec2 <- sapply(x.vec2, ftn)
  S <- h/3*(ftn(a) + ftn(b) + 4*sum(f.vec1) + 2*sum(f.vec2))
  return(S)
}


ftn6<-function(x) return(4*x**3)


#PARA COMPARAR GRAFICAMENTE  SIMPSON CON GEOGEBRA
a<-0
b<-9
n<-1
ftn7<-function(x) return(1.25*sin(x/2)+3)

simpson_n(ftn6,a,b,10)
simpson_n(ftn7,a,b,n)
curve(1.25*sin(x/2)+3,xlim=c(0,10),ylim=c(0,4.5))
abline(v=a,col=2)
abline(v=b,col=3)
trapezoid(ftn7,a,b,3)
