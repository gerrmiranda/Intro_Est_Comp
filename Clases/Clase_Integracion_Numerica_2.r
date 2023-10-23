# program spuRs/resources/scripts/Phi.r
# estimate and plot the normal cdf Phi
library(here)

rm(list = ls()) # clear the workspace
source(here("Scripts","Int. Numérica y Cuad. Gaussiana","simpson_n.r"))

phi <- function(x) return(exp(-x^2/2)/sqrt(2*pi))
Phi <- function(z) {
  if (z < 0) {
    return(0.5 - simpson_n(phi, z, 0,n))
  } else {
    return(0.5 + simpson_n(phi, 0, z,n))
  }
}

a<--5
b<-5
z <- seq(a,b, by = 0.1)
n<-50
phi.z <- sapply(z, phi)
Phi.z <- sapply(z, Phi)
plot(z, Phi.z, type = "l", ylab = "",col=2, main = expression(paste("Densidad: ", phi(z), " Distribución: ",Phi(z))))
lines(z, phi.z,col=4)
legend("topleft",paste("Distribución aproximada \n Regla Simpson",n))

Phi.z_2<-Phi.z
Phi.z_10<-Phi.z
Phi.z_50<-Phi.z
plot(z,Phi.z_2-Phi.z_10,type="l")
abline(h=0)
abline(v=0)
plot(z,Phi.z_10-Phi.z_50,type="b")

plot(Phi.z_2,Phi.z_10)

