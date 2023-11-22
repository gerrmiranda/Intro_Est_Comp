# Bajo condición: - cantidad de clientes conocida
#                 - planes viejos conocidos


# recomendación: - no usar set.seed

# Condiciones 
n_ini=100
min=200
max=3000
n=20

p_actuales=as.integer(runif(n_ini,min,max))
q_actuales=as.integer(runif(n_ini,1,1000))

p_actuales

o_pact=order(p_actuales)
p_actuales=p_actuales[o_pact]
p_actuales
Ingresos_acutuales <- sum(p_actuales*q_actuales)


# 1 fucino q cree ran 20 planes
 
# 2 fun q vea la ganancia

# 3  simular y ¿graficar?



# 1

planes_nuv <- function(n,min,max) {
    # n cantidad de planes
    # min precio para crear planes
    # max precio para crear planes
    # devuelve n planes nuevos generados por runif
    p_nuevos_L = as.integer(runif(20,min,max))
    o_pnuev_L = order(p_nuevos_L)
    p_nuevos_L = p_nuevos_L[o_pnuev_L]
    return(p_nuevos_L)
}



# 2

ganancia_nuv <- function(new_p,old_p,old_cli){
    # new_p vector planes nuevos 
    # old_p vector planes viejos
    # old_cli vector actuales
    # devuelve: - distribucion nueva de los clientes
    #           - cuanto es la ganancia del plan nuevo
    #           - la ganancia del plan viejo 
    #           - la diferencia entre ambas
    
    # nueva distrivucion de los clientes
    new_cli=0
    for (j in 1:length(new_p)) {
        q=0
        for (i in 1:length(old_p)) {
            if (old_p[i]>=new_p[j]) {
                q = q + old_cli[i]
                new_cli[j]= q
            }
        }
    }
    for (i in 1:(length(new_cli)-1)) {
        new_cli[[i]] = new_cli[[i]] - new_cli[[i+1]] 
    }
    
    # nuevas ganacias
    new_ingresos <- sum(new_p*new_cli)
    
    # viejas ganancias 
    old_ingresos <- sum(old_p*old_cli)
    
    #diferencia (asumiendo old_ingresos > new_ingresos)
    diferencia = old_ingresos - new_ingresos
    
    res <- list(new_cli = new_cli , 
                new_ingresos = new_ingresos , 
                old_ingresos = old_ingresos , 
                diferencia = diferencia)
    return(res)
}



# 3

# Simulador

simul_L <- function(n,min,max,old_p,old_cli,tol,cant_sim = 1000){
    # usa planes_nuv y ganancia_nuv
    # tol valor que hay que superar para ser aceptada la nueva distribución
    # cant_sim cantidad de simulaciones
    # devuelve: - 
    
    all <- matrix(0 , nrow = cant_sim , ncol = 22)
    # primeras 20 columnas son los 20 planes 
    # columna 21 es la diferencia con los ingresos viejos
    # columna 22 es un T/F según se alcance la tolerancia
    
    for (i in 1:cant_sim) {
        plan_i <- planes_nuv(n,min,max)
        gan_i <- ganancia_nuv(plan_i,old_p,old_cli)
        t_f <- as.numeric(gan_i$diferencia>tol)
        all[i,] <- c(plan_i,gan_i$diferencia,t_f)
    }
    return(all)
}


res_sim <- simul_L(n,min,max,p_actuales,q_actuales,1000000)
View(res_sim)



