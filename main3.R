fun<-function(x,mu,sigma){
    fun=(1/sqrt(2*pi*sigma))*exp((-(x-mu)^2)/(2*sigma^2))
}
x=seq(-4,4,0.01)
plot(x,fun(x,0,1))