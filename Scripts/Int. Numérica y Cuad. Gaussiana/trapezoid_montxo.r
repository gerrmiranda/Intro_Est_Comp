# program spuRs/resources/scripts/trapezoid.r

trapezoid <- function(ftn, a, b, n = 100) {
  # numerical integral of ftn from a to b
  # using the trapezoid rule with n subdivisions
  #
  # ftn is a function of a single variable
  # we assume a < b and n is a positive integer
  
  h <- (b-a)/n
  x.vec <- seq(a, b, by = h)
  f.vec <- sapply(x.vec, ftn)
  T <- h*(f.vec[1]/2 + sum(f.vec[2:n]) + f.vec[n+1]/2)
  return(T)
}

ftn6<-function(x) return(4*x**3)

primitiva<-function(x) return(x**4)



a<-0
b<-1
n<-10
h<-(a+b)/n
h0<-c(a,0)
h1<-seq(a,b,h)
rango<-seq(a,2*b,0.001)
plot(rango,(ftn6(rango)),type="l",xlim=c(a,b+1),ylim=c(a,ftn6(b)+1),ylab="f(x)")
abline(v=b,col=2)
trapezoid(ftn6,a,b,n)
trapezoid(ftn6,0,1,n)
(Error=trapezoid(ftn6,0,1,n)-(primitiva(b)-primitiva(a)))
legend(b*(1.1),ftn6(b)+1,paste("Error",Error))
segments(h0,ftn6(h0),h,ftn6(h))
segments(h,ftn6(h),2*h,ftn6(2*h))
