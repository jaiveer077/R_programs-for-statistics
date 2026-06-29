dbinom(0:3,5,0.5)
pbinom(3,5,0.5)
pbinom(0:3,5,0.5)
qbinom(0.25,10,0.5)
qbinom(c(0.25,0.5,0.75),10,0.5)
pbinom(c(3:7),10,0.5)
rbinom(10,4,0.5)
rbinom(10,4,0.5)

dbinom(6,8,0.65)
pbinom(5,8,0.65)
rbinom(50,10,1/6)
table(rbinom(50,10,1/6))
x<-0:5
x
p<-dbinom(x,5,0.42)
p
sum(p)
weighted.mean(x,p)
weighted.mean(x^2-2.1,p)
weighted.mean(x^2-2.1,p)^0.5
hist(p)
x<-0:10
barplot(dbinom(0:10,10,0.5),
        names.arg = x,
        col = "skyblue",
        xlab = "Number of successes",
        ylab = "P(X=x)",
        main = "Binomial Distribution PMF (n=10,p=0.5)")
cdf<-pbinom(x,10,0.5)
cdf
barplot(cdf,names.arg=x,col="skyblue",xlab="Number of successes(x)",ylab="P(X<=x)",main="Cumulative Binomial Distribution")
dpois(0:3,5)
ppois(0:3,5)
qpois(0.7,5)
qpois(c(0.25,0.5,0.75),5)
ppois(3:7,5)
rpois(10,5)

dpois(4,6)
# atleast 4 calls in 5 minute-period
1-ppois(3,6)
x<-0:15
probs<-dpois(x,6)
barplot(probs,names.arg=x,col="skyblue",xlab="Number of calls",ylab="Probability",main="Possion Distribution (Lambda=6)")
rpois(50,6)
table(rpois(50,6))

1-pbinom(1,7,0.25)
p<-0.25
n<-1
while((1-dbinom(0,n,p))<=2/3){
  n<-n+1
}
n

dpois(0,5/3)
1-ppois(1,5/3)
dbinom(5,300,0.01)
dpois(5,3)

dnbinom(3,4,0.6)
dnbinom(0:5,4,0.6)
pnbinom(3,4,0.6)
pnbinom(0:5,4,0.6)

qnbinom(c(0.25,0.5,0.75),4,0.6)
pnbinom(0:5,4,0.6)
rnbinom(10,4,0.6)
rnbinom(10,4,0.6)

dnbinom(25,6,0.2)
1-pnbinom(19,6,0.2)
y<-rnbinom(1000,6,0.2)
hist(y)


dgeom(5,0.5)
dgeom(0:4,0.15)
pgeom(5,0.5)
pgeom(0:4,0.15)
qgeom(0.7,0.2)
qgeom(c(0.25,0.5,0.75),0.2)
pgeom(1:6,0.2)
rgeom(10,0.2)

dgeom(8,0.13)
pgeom(10,0.13)-pgeom(5,0.13)
rgeom(200,0.13)
table(rgeom(200,0.13))
summary(p)
barplot(y)


dnbinom(6-4,4,0.8)
dgeom(2,0.8)

dbinom(6-4,4,0.55)
1-pbinom(3,7,0.55)
1-pbinom(2,5,0.55)
