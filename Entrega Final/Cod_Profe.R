new_cli <- c(7168, 190, 6704, 2999, 77, 8228, 938, 3548, 3393, 515, 0, 4544, 1174, 707, 0, 349, 410, 0, 752, 0)
# Ingreso inicial fijo
i_inicial <- 43777406

# Función de ingresos
dif_ingresos <- function(new_p) {
  i <- sum(new_p * new_cli)
  i <- i_inicial - i
  return(i)
}

new_p <- c(268, 435, 467, 541, 642, 685, 1095, 1253, 1436, 1696, 1897, 1938, 1991, 2230, 2499, 2510, 2638, 2960, 2969, 2999)

# Límites para los precios de los planes
lower_bound <- rep(200, length(new_p))
upper_bound <- rep(3000, length(new_p))

res <- optim(par = new_p, fn = dif_ingresos, method = 'L-BFGS-B', lower = lower_bound, upper = upper_bound)

# Precios óptimos encontrados
precios_optimos <- res$par
precios_optimos