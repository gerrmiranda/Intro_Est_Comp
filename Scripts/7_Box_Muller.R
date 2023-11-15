#algoritmo de Box MUller
t<-10000
x<-matrix(0,t,6)
set.seed(1234)
x[,1]<-runif(t)
x[,2]<-runif(t)
x[,3]<-sqrt(-2*log(x[,1]))*cos(2*pi*x[,2])
x[,4]<-sqrt(-2*log(x[,1]))*sin(2*pi*x[,2])
x[,5]<-rnorm(t)
x[,6]<-rnorm(t)
head(x)
summary(x[,3:4])
par(mfrow=c(1,2))
hist(x[,5],probability = TRUE, ylab=c(0,1.5),breaks = 100)
lines(density(x[,3]),col="red")
hist(x[,6],probability = TRUE, ylab=c(0,1.5),breaks = 100)
lines(density(x[,4]),col="blue")
par(mfrow=c(1,1))
plot(x[,3],x[,4],xlab="N1",ylab="N2",pch=1)
plot(x[,5],x[,6],xlab="N1",ylab="N2",pch=1)
cor(x[,3],x[,4])

test3<-function(){x[,1]<-runif(t)
x[,2]<-runif(t)
x[,3]<-sqrt(-2*log(x[,1]))*cos(2*pi*x[,2])
x[,4]<-sqrt(-2*log(x[,1]))*sin(2*pi*x[,2])}

test4<-function(){
  x[,5]<-rnorm(t)
  x[,6]<-rnorm(t)
}

system.time(test3());system.time(test4())

par(mfrow=c(2,2))
qqplot(x[,3],x[,5],main="qqplot x[,3] y x[,5] ", cex.main=0.9)
qqplot(x[,4],x[,5],main="qqplot x[,4] y x[,6] ", cex.main=0.9)
qqnorm(x[,3],main="qqnorm x[,3]", cex.main=0.9, col=2)
qqnorm(x[,5],main="qqnorm x[,5]", cex.main=0.9, col=3)

t<-100
x<-matrix(0,t,6)
set.seed(1234)
x[,1]<-runif(t)
x[,2]<-runif(t)
x[,3]<-sqrt(-2*log(x[,1]))*cos(2*pi*x[,2])
x[,4]<-sqrt(-2*log(x[,1]))*sin(2*pi*x[,2])
x[,5]<-rnorm(t)
x[,6]<-rnorm(t)
par(mfrow=c(1,1))

plot(ecdf(x[,3]),main=" ECDF", cex.main=0.9)
lines(ecdf(x[,4]),col=2)
lines(ecdf(x[,5]),col=3)
lines(ecdf(x[,6]),col=4)
 