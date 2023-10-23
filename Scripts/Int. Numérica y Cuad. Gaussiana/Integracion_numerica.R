# Aca podemos usar

trapezoid_montxo.r#  Regla del trapecio y su puede comparar con geogebra
simpson_n_montxo.r# Regla de Simpson. puede comparar con geogebra
simpson_montxo.r # Regla de Simpson  Tolerancia prefijada

simpson_test.r #Se estudia Convergencia

Phi_montxo.r # Para construir la curva de Gauss puede comparar con geogebra



# ejemplos de función  integrate

integrand <- function(x) {1/((x+1)*sqrt(x))}
integrand
abcisas<-seq(-0,4,0.01)
plot(abcisas,integrand(abcisas))
integrate(integrand, lower = 0.5, upper = 1.5)
integrate(integrand, lower = 0, upper = Inf)


phi <- function(x) return(exp(-x^2/2)/sqrt(2*pi))

integrate(phi, lower = 0, upper = Inf)


#Cuadratura adaptativa
options(digits = 16)



a<-0;b<-1
k<-4

f4<-function(x) 5*x**4
simpson(f4,a,b,tol=1e-9,verbose = TRUE)
#partition size 512 
#[1] 1.000000000009701


k<-8
f8<-function(x) (k+1)*x**(k)
simpson(f8,a,b,tol=1e-9,verbose = TRUE)
#partition size 1024 
#[1] 1.000000000015279

k<-12
f12<-function(x) (k+1)*x**(k)
simpson(f12,a,b,tol=1e-9,verbose = TRUE)
#partition size 2048 
#[1] 1.000000000005419


#ahora cambiamos la forma de hacer la integracion, teniendo en cuenta que la funcion
#  (k+1)*x**(k) es mucho mas empinada en el intervalo [0.5,1] que en [0,0.5]

# por ejemplo hacerlo separada (adaptativo) es mejor que hacerlo en on intervalo [0,1]

S1<-simpson(f12,0,0.5,tol=5e-10,verbose = TRUE)
S2<-simpson(f12,0.5,1,tol=5e-10,verbose = TRUE)

S1+S2


#ejemplo para 
ftn<-function(x) return(1.5*sqrt(x))
simpson(ftn,a,b,tol=0.001,verbose = TRUE)
simpson1<-simpson(ftn,a,b,tol=1e-9,verbose = FALSE)
(tiempo.simpson1<-system.time(simpson(ftn,a,b,tol=1e-9,verbose = FALSE)))
cuadratura1<-quadrature(ftn,a,b,tol=1e-9,trace=FALSE)
(tiempo.cuadratura1<-system.time(quadrature(ftn,a,b,tol=1e-9,trace=FALSE)))
cuadratura1[1]-simpson1


ftn <- function(x) return(1.5*sqrt(x))
quadrature(ftn, 0, 1, tol = 1e-3, trace = TRUE)
