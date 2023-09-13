#Impuestos a las galletas
impuesto_galletas <- function(gasto){
    if(5<gasto & gasto<=10){
        gasto*0.1
    }else if(gasto>10 & gasto<=20){
        gasto*0.15
    }else if(gasto>20){
        gasto*0.25
    }else{0} #acá solo entra cuando el gasto es menor o igual a 5
}


#Fibonacci
fibonacci <- function(k){  #Primeros k elementos de la secuencia de Fibonacci
    cuant <- rep(0,k)
    for (i in 1:k){
        if(i==1){
            cuant[i] <- 0
        }else if(i==2){
            cuant[i] <- 1
        }else{
            cuant[i] <- cuant[i-1] + cuant[i-2]
        }
    }
    return(cuant)
}

#Primer Fibonacci mayor que 100
#Para hacerlo de forma diferente a la presentación usaremos la función anterior
#Será una función genérica que mientras no encuentre el valor mayor que k volverá a hacer un Fibonacci con un valor más (sumamente ineficiente pero diferente)
primero_mayor_k <- function(k){
    res=0
    i=0
    while(res<k){
        secuencia <- fibonacci(i+1)
        res <- secuencia[i+1]
        i=i+1
    }
    return(res)
}


#Fórmula de Viéte para calcular pi

#El siguiente procedimiento parecía más lógico para luego graficar.
#Fórmula de Viéte con k raíces cuadradas pero k tiene que ser igual o mayor que 3
viete_k <- function(k){
    rai <- sqrt(2)
    for(i in 3:k){
        rai <- sqrt(2+rai)
    }
    res <- ((2^k)*sqrt(2-rai))
    return(res)
}

viete_k <- function(k) {
    rai = sqrt(2)
    
    rai = sqrt
}

#Con la función anterior ahora se calcula cual k es el mínimo necesario para tener una precisión de 5 dígitos decimales
tolerancia <- 1e-5
precision <- 1
k <- 3

while(precision > tolerancia){
    aproximación <- viete_k(k)
    k <- k+1
    precision <- abs(aproximación-pi)
}
resultado=k-1
resultado


pres = function (x) {
    aproximación <- viete_k(x)
    precision <- abs(aproximación-pi)
    return(precision)
}

y=list()
for (i in 3:resultado) {
    y[i]=pres(i)
}


x=seq(3,9,1)

pres(x)

library(ggplot2)

plot(x=equis,y=y)

