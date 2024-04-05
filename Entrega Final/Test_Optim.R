
new_p =c(268,  435,  467,  541,  642,  685, 1095 ,1253 ,1436, 1696, 1897, 1938, 1991, 2230, 2499, 2510, 2638, 2960, 2969, 2999)


dif_ingresos= function(new_p) {
  i_inicial=43777406
  new_cli=c(7168,190, 6704 ,2999,   77, 8228,  938, 3548, 3393,  515,    0, 4544, 1174,  707,    0,  349,  410,   0 , 752,    0)
  i=sum(new_p*new_cli)
  i=i_inicial-i
  return(i)}  


res <- optim(new_p, fn=dif_ingresos,method = 'SANN',
             control = list(maxit = 3600,  trace = TRUE,
                            REPORT = 500))