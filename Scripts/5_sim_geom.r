Ni <- rgeom(10, prob = 1/2); table(factor(Ni, 0:max(Ni)))

sim_geom<-function(n=10,p){
#  n=10
  simula<-matrix(0,n,1,byrow = TRUE)
  for (i in 1:n)
  {
    sim<-0
    u<-0
      while (u<1-p)
      {u<-runif(1)
      print(u)
      sim<-sim+1
      print(sim)
      }
    simula[i]<-sim
    print(simula[i])}
   return(simula)
}


sim_geom(10,0.5)
