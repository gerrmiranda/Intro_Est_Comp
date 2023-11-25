# Bajo condición: - cantidad de clientes conocida
#                 - planes viejos conocidos


# recomendación: - no usar set.seed

# Condiciones 
n_ini=100
min=200
max=3000

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

planes_nuv <- function(n_final,min,max) {
    # n_final cantidad de planes menos dos
    # min precio para crear planes y el primer plan
    # max precio para crear planes y el último plan
    # devuelve n planes nuevos generados por runif
    p_nuevos_L = as.integer(runif(n_final,min+1,max-1))
    p_nuevos_L = p_nuevos_L[order(p_nuevos_L)]
    p_nuevos_L = c(min,p_nuevos_L,max)
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
    new_cli=rep(0,length(new_p))
    
    for(i in length(old_cli):1) {
        j = length(new_p)
        while(old_p[i]<new_p[j] && j!=1){
            j=j-1
        }
        new_cli[j] = new_cli[j] + old_cli[i]
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
n_final = 18


simul_L <- function(n_final,min,max,old_p,old_cli,tol,cant_sim = 1000){
    # usa planes_nuv y ganancia_nuv
    # tol valor que hay que superar para ser aceptada la nueva distribución
    # cant_sim cantidad de simulaciones
    
    all <- matrix(0 , nrow = cant_sim , ncol = (n_final + 4))
    # primeras n_final+2 columnas son los 20 planes 
    # penúltima columna es la diferencia con los ingresos viejos
    # última columna es un T/F según se alcance la tolerancia
    
    for (i in 1:cant_sim) {
        plan_i <- planes_nuv(n_final,min,max)
        gan_i <- ganancia_nuv(plan_i,old_p,old_cli)
        t_f <- as.numeric(gan_i$diferencia<tol)
        all[i,] <- c(plan_i,gan_i$diferencia,t_f)
    }
    return(all)
}


res_sim <- simul_L(n_final=n_final,min,max,p_actuales,q_actuales,-600000)
View(res_sim)




summary(res_sim[,21])

plot(res_sim[,21])




# Simular con distintas cantidades de planes


simu_cant <- function(n_start,n_end,min,max,old_p,old_cli,tol,cant_sim = 1000) {
    # crea una matris con los datos del summary de la diferencia de matrices de la función simul_L
    # n_start es la cantidad de planes inicial y n_end la final, n_end > n_start1
    mat <- matrix(0, nrow = n_end-n_start+1 , ncol = 8)
    for(n in n_start:n_end){
        datos = simul_L(n,min,max,old_p,old_cli,tol)
        mat[(n-n_start+1),] = c(n,t(as.matrix(summary(datos[,n+3]))),sum(datos[,n+4]))
    }
    return(mat)
}

mat <- simu_cant(18,80,200,3000,p_actuales,q_actuales,1000000)
View(mat)


#Recordad que estamos viendo la diferencia, entonces son interesa que san lo mínimo posible














