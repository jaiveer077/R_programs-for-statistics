cgpa<-matrix(c(7.1,8.2,6.5,9.0,7.8,8.4,6.9,7.3,8.1,7.7,
               6.8,7.5,8.9,9.2,7.0,6.7,8.3,7.4,8.6,7.9,
               7.2,6.6,8.0,9.1,7.6,8.5,6.4,7.8,8.7,7.3,
               7.9,8.1,6.8,7.0,8.4,9.3,7.2,6.9,8.8,7.5),nrow=4,ncol=10,byrow=TRUE)
print(cgpa)
mean_val<-sum(cgpa)/length(cgpa)
mean_val

1-pbinom(4,20,0.10)
pbinom(5,20,0.10)
val<-pbinom(7,20,0.10)-pbinom(2,20,0.10)
val
for(k in 0:10){
  if(pbinom(k,20,0.10)>=0.70){
    print(k)
    break;
  }
}

f<-function(x){
  if(x<0){
    return (2*x^2)
  } else if(x>=0){
    return (sqrt(x))
  }
}
x_vals<- -4:4
result<-numeric(length(x_vals))
for(i in 1:length(x_vals)){
  result[i]<-f(x_vals[i])
}
result
x<-seq(-4,4,0.01)
y<-sapply(x,f)
plot(x,y,type="l",xlab="x",ylab="f(x)",main="Plot of piecewise function")
install.packages("qcc")
library(qcc) 
defects<-c(22,17,13,9,4)
names(defects)<-c("Incorrect dimensions","Missing components","Electrical fault","Paint bubbles","Labelling errors")
pareto.chart(defects,xlab="Defect type",ylab="No of defect",col=heat.colors(length(defects)),main="Pareto chart of Product defects")

defects[which.max(defects)]
defects[which.max(defects)]

r_func<-function(n,p,r){
  x<- 0:n
  pdf<-dbinom(x,n,p)
  moment<-sum((x^r)*pdf)
  moment
}
n<-25
p<-0.6
third_moment<-r_func(n,p,3)
third_moment
mean_val<-r_func(n,p,1)
mean_val
second_moment<-r_func(n,p,2)
second_moment
variance<-second_moment-mean_val^2
variance


p<-0.15
k<-4
result<-dnbinom(2,k,p)
result
n<-10
prob<-pbinom(3,n,p)
prob

p<-0.15
for(n in 4:200){
  prob<-1-pbinom(3,n,p)
  if(prob>=0.90){
    print(n)
    break
  }
}