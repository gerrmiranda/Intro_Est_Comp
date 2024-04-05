#codigo matlab::
#r=1000
#a=zeros(r,1);
#b=zeros(r,1);
#for i=1:r
#c(i)=unidrnd(6);
#v(i)=unidrnd(6);
#a(i)=(c(i)<=5);
#end
#abar=mean(a)
#asig=var(a)
#bbar=mean(b)
#bsig=var(b)
#b(i)=(c(i)<=v(i));




set.seed(1234)
r=10000
a=matrix(0,r,1)
b=matrix(0,r,1)
c=matrix(0,r,1)
v=matrix(0,r,1)
rdu<-function(n,k) sample(1:k,n,replace=TRUE)

for (i in 1:r)
{c[i]=rdu(1,6)
v[i]=rdu(1,6)
a[i]=(c[i]<=5)
b[i]=(c[i]<=v[i])
}

abar=mean(a)
asig=var(a)
bbar=mean(b)
bsig=var(b)


tabla<-as.data.frame(matrix(0,5,3))
 colnames(tabla)<-c("parametro","valor exacto","valor estimado")
rownames(tabla)<-NULL

tabla[1,1]=c("abarrra")
tabla[2,1]=c("Sigma2 a")
tabla[3,1]=c("bbarrra")
tabla[4,1]=c("Sigma2 b")
tabla[5,1]=c("simulaciones")
tabla[1,2]=0.8333
tabla[2,2]=0.1389
tabla[3,2]=0.5833
tabla[4,2]=0.2431

tabla[1,3]=abar
tabla[2,3]=asig
tabla[3,3]=bbar
tabla[4,3]=bsig
tabla[5,3]=r
 
Abar<-matrix(0,r,2)
for (i in 1:nrow(Abar))
{Abar[i,1]<-mean(a[1:i])
Abar[i,2]<-var(a[1:i])}

par(mfrow=c(2,2))
plot(Abar[,1],type="l",xlab="r",cex.main=0.9,main="Media en el muestreo de a")
abline(h=0.8333,col=2)
plot(Abar[,2],type="l",xlab="r",cex.main=0.9,main="Varianza en el muestreo de a")
abline(h=0.1389,col=2)

Bbar<-matrix(0,r,2)

for (i in 1:nrow(Bbar))
{Bbar[i,1]<-mean(b[1:i])
Bbar[i,2]<-var(b[1:i])}


plot(Bbar[,1],type="l",xlab="r",cex.main=0.9,main="Media en el muestreode b")
abline(h=0.5833,col=2)
plot(Bbar[,2],type="l",xlab="r",cex.main=0.9,main="Varianza en el muestreo de b")
abline(h=0.2431,col=2)


a <- round(runif(1000, min=0, max=100))
a <- ceiling(runif(1000, min=0, max=100))


dunifdisc<-function(x, min=0, max=1) ifelse(x>=min & x<=max & round(x)==x, 1/(max-min+1), 0)
punifdisc<-function(q, min=0, max=1) ifelse(q<min, 0, ifelse(q>max, 1, floor(q)/(max-min+1)))
qunifdisc<-function(p, min=0, max=1) floor(p*(max-min+1))
runifdisc<-function(n, min=0, max=1) sample(min:max, n, replace=T)