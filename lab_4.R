x<-c(1,2,3,4,5,6)
p<-c(1/6,1/6,1/6,1/6,1/6,1/6)
exp<-sum(x*p)
exp
weighted.mean(x*p)
exp2<-sum(x^2*p)
exp2

f<-function(x){
  2000/x^3
}
pdf<-integrate(f,100,Inf)
pdf$value

ex<-integrate(function(x) x*f(x),100,Inf)
ex$value 

x<-c(0,1,2,3,4)
probx<-c(0.41,0.37,0.16,0.05,0.01)
avg<-sum(x*probx)
avg
varix=sum(x^2*probx)-(weighted.mean(x,probx))^2
varix

pdf<-function(x,lam){
  pdf=lam*exp(-lam*x)
  pdf
}
expec<-function(lam,r){
  int=function(x){
    int=(x^r)*pdf(x,lam)
    int
  }
  expec=integrate(int,0,Inf)
  expec
}
expec(2,1)
expec(2,2)-expec(2,1)^2
expec(2,3)

# Mean and variance of binomial distribution 
pdf<-function(x,n,p){
  pdf=choose(n,x)*p^x*(1-p)^(n-x)
  pdf
}
expec<-function(n,p,r){
  x=0:n
  int<-function(x){
    int=(x^r)*pdf(x,n,p)
    int
  }
  expec=sum(int(x))
  expec
}
expec(30,0.4,1)
expec(30,0.4,2)-(expec(30,0.4,1)^2)

pdf<-function(x,mu,sigma){
  pdf=1/(sigma*(sqrt(2*pi)))*exp(-0.5*((x-mu)/sigma)^2)
  pdf
}
expec<-function(mu,sigma,r){
  int<-function(x){
    int=(x^r)*pdf(x,mu,sigma)
    int
  }
  expec=integrate(int,-Inf,Inf)$value
  expec
}
expec(12,4,1)
expec(12,4,2)
expec(12,4,3)
