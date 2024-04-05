blineal<-function(FUN,dFUN,xn,pn,a_max=1,tau=0.5,maxiter_bt=10,c1=1e-4,c2=0.9,tau2=0.8,mostrar_bl=TRUE){
	phi<-function(x,a,p) FUN(x+a*p)
	phi0<-phi(xn,0,pn)
	alfa<-a_max
	phia<-phi(xn,alfa,pn)

	# condicion de descenso suficiente
	iter_bt<-1
	armijo<-phi0+c1*alfa*sum(pn^2)
	while(phia<=armijo & iter_bt<maxiter_bt){
		alfa<-alfa*tau
		armijo<-phi0+alfa*c1*sum(pn^2)
		phia<-phi(xn,alfa,pn)
		iter_bt<-iter_bt+1
	}

	# condicion de curvatura
	iter_ft<-0
	while(sum(dFUN(xn+alfa*pn)*pn) > c2*sum(pn^2)){
		if(iter_ft==0 & mostrar_bl==TRUE) cat('   Realizando forward-tracking','\n')
		iter_ft<-iter_ft+1
		alfa_old<-alfa/tau
		dif<-alfa_old - alfa
		alfa<-alfa + dif*(1-tau^iter_ft)
	}

	if(iter_bt==maxiter_bt & mostrar_bl==TRUE) cat('   Fallo la busqueda lineal','\n')
	salida<-list(alfa=alfa,iter_bt=iter_bt,iter_ft=iter_ft)
	return(salida)
}


ascenso<-function(FUN,dFUN,x0,tol=1e-5,maxiter=1000,mostrar=TRUE,control=list(a_max=1,tau=0.5,maxiter_bt=10,c1=1e-4,c2=0.9,tau2=0.8,trace_bl=mostrar)){
	pn <- dFUN(x0)
		alfa<-blineal(FUN,dFUN,x0,pn,a_max=control$a_max,tau=control$tau,maxiter_bt=control$maxiter_bt,c1=control$c1,c2=control$c2,tau2=control$tau2,mostrar_bl=control$trace_bl)
	an<-alfa$alfa
	x1<-x0+an*pn
	iter<-1
	cambio<-sum(pn^2)

	while(cambio>tol & iter<maxiter){
		x0<-x1
		pn<-dFUN(x0)
		alfa<-blineal(FUN,dFUN,x0,pn,a_max=control$a_max,tau=control$tau,maxiter_bt=control$maxiter_bt,c1=control$c1,c2=control$c2,tau2=control$tau2,mostrar_bl=control$trace_bl)
		an<-alfa$alfa
		x1<-x0+an*pn
		if(mostrar==TRUE) cat(paste('x',iter,sep=''),x1,'\n')
		iter<-iter+1
		cambio<-sum(pn^2)
	}
	if(iter==maxiter & mostrar==TRUE) cat('No se alcanzo convergencia','\n')

	salida<-list(x_max=x1,fx_max=FUN(x1),iter=iter)
	return(salida)
}


f1<-function(x){
	x1<-x[1]
	x2<-x[2]
	sin(0.5*x1^2-0.25*x2^2)*cos(2*x1-exp(x2))
}

df1<-function(x){
	x1<-x[1]
	x2<-x[2]
	dx1<-      x1*cos(0.5*x1^2-0.25*x2^2)*cos(2*x1-exp(x2)) -       2*sin(0.5*x1^2-0.25*x2^2)*sin(2*x1-exp(x2))
	dx2<- -0.5*x2*cos(0.5*x1^2-0.25*x2^2)*cos(2*x1-exp(x2)) + exp(x2)*sin(0.5*x1^2-0.25*x2^2)*sin(2*x1-exp(x2))
	return(c(dx1,dx2))
}

a1<-ascenso(f1,df1,c(0.1,0.3))
str(a1)
a1
# luego de usar el image del otro script
points(a1$x_max[1],a1$x_max[2],pch=16,col='white')

a2<-ascenso(f1,df1,c(0,0.5))
# en el mismo image
points(a2$x_max[1],a2$x_max[2],pch=16,col='white')
