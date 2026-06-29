fun<-function(x,lam){
  fun=lam*exp(-lam*x)
}
x=seq(0,10,0.001)
plot(x,fun(x,1))
lines(x,fun(x,5))
lines(x,fun(x,8))