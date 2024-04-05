f1<-function(x) x-x*log(x)-exp(-x)

x<-seq(0.01,5,0.01)
plot(x,f1(x),type='l')
abline(h=0)

gs2<-function(FUN,xi,xd,xm=NULL,tol=1e-5,maxiter=1000,trace=TRUE){
  amplitud<-Inf
  if(is.null(xm)) xm<-(xi+xd)/(1+1)
  fi<-FUN(xi)
  fd<-FUN(xd)
  fm<-FUN(xm)
  points(xi,fi,col=2)
  points(xd,fd,col=4)
  iter<-1
  while(amplitud>tol & iter<maxiter){
    if(xd-xm > xm-xi){
      xk<-xm+(xd-xm)/(1+1)
      fk<-FUN(xk)
      if(fk>=fm) {
        xi<-xm; fi<-fm
        xm<-xk; fm<-fk
      } else {
        xd<-xk; fd<-fk
      }
    } else {
      xk<-xm-(xm-xi)/(1+1)
      fk<-FUN(xk)
      if(fk>=fm) {
        xd<-xm; fd<-fm
        xm<-xk; fm<-fk
      } else {
        xi<-xk; fi<-fk
      }			
    }
    iter<-iter+1
    amplitud <-xd-xi
    points(xk,fk,col=3)
    if(trace) cat('La amplitud del intervalo es',amplitud,'\n')
 #   print(xk) 
  }
  if(iter==maxiter) cat('Se alcanzo el numero maximo de iteraciones','\n')
  
  salida<-list(x_max=xm, fx_max=fm, iter=iter,amplitud=amplitud)
  return(salida)
}


out1<-gs2(f1,0.05,2)

gsr<-function(FUN,xi,xd,xm=NULL,tol=1e-5,maxiter=1000,trace=TRUE){
  amplitud<-Inf
  #set.seed(1234)
  xm<-xm+runif(1)*(xd-xm)
  fi<-FUN(xi)
  fd<-FUN(xd)
  fm<-FUN(xm)
  points(xi,fi,col=2)
  points(xd,fd,col=4)
  iter<-1
  while(amplitud>tol & iter<maxiter){
    if(xd-xm > xm-xi){
      xk<-xi+runif(1)*(xd-xi)
      fk<-FUN(xk)
      if(fk>=fm) {
        xi<-xm; fi<-fm
        xm<-xk; fm<-fk
      } else {
        xd<-xk; fd<-fk
      }
    } else {
      xk<-xm-runif(1)*(xm-xi)
      fk<-FUN(xk)
      if(fk>=fm) {
        xd<-xm; fd<-fm
        xm<-xk; fm<-fk
      } else {
        xi<-xk; fi<-fk
      }			
    }
    iter<-iter+1
    amplitud <-xd-xi
    points(xk,fk,col=3)
    if(trace) cat('La amplitud del intervalo es',amplitud,'\n')
  print(xk)  
  }
  if(iter==maxiter) cat('Se alcanzo el numero maximo de iteraciones','\n')
  
  salida<-list(x_max=xm, fx_max=fm, iter=iter,amplitud=amplitud)
  return(salida)
}

out2<-gsr(f1,0.05,2)


gs<-function(FUN,xi,xd,tol=1e-5,maxiter=1000,trace=TRUE){
  amplitud<-Inf
  ro<-(1+sqrt(5))/2
  #	if(is.null(xm)) xm<-xi+(xd-xi)/(1+ro)
  xm<-xi+(xd-xi)/(1+ro)
  fi<-FUN(xi)
  fd<-FUN(xd)
  fm<-FUN(xm)
  points(xi,fi,col=2)
  points(xd,fd,col=4)
  iter<-1
  while(amplitud>tol & iter<maxiter){
    if(xd-xm > xm-xi){
      xk<-xm+(xd-xm)/(1+ro)
      fk<-FUN(xk)
      if(fk>=fm) {
        xi<-xm; fi<-fm
        xm<-xk; fm<-fk
      } else {
        xd<-xk; fd<-fk
      }
    } else {
      xk<-xm-(xm-xi)/(1+ro)
      fk<-FUN(xk)
      if(fk>=fm) {
        xd<-xm; fd<-fm
        xm<-xk; fm<-fk
      } else {
        xi<-xk; fi<-fk
      }			
    }
    iter<-iter+1
    amplitud <-xd-xi
    points(xk,fk,col=3)
    if(trace) cat('La amplitud del intervalo es',amplitud,'\n')
  print(xk)  
  }
  if(iter==maxiter) cat('Se alcanzo el numero maximo de iteraciones','\n')
  
  salida<-list(x_max=xm, fx_max=fm, iter=iter,amplitud=amplitud)
  return(salida)
}
out3<-gs(f1,0.05,2)
