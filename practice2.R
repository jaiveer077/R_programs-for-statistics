p<-0.08
prob<-dgeom(5-1,p)
prob

prob_b<-pgeom(10-1,p)
prob_b

for(k in 1:200){
  if(pgeom(k-1,p)>=0.90){
    print(k)
    break
  }
}

sim_days<-rgeom(100,p)+1
barplot(table(sim_days),main="Simulation of days until first failure",xlab="Days",ylab="Frequency",col="skyblue")


population<-1:200
without_replacement<-sample(population,size=20,replace=FALSE)
with_replacement<-sample(population,size=20,replace=TRUE)

mean_without<-mean(without_replacement)
var_without<-var(without_replacement)
mean_without
var_without

mean_with<-mean(with_replacement)
var_with<-var(with_replacement)
mean_with
var_with

score<-c(45,60,72,88,91,55,67,73,84,69)
mean_val<-mean(score)
mean_val
med_val<-median(score)
med_val
std_val<-sd(score)
std_val
sorted_score<-sort(score)
sorted_score
summary(score)

freq_tab<-table(score)
freq_tab

mode_val<-names(sort(-table(score)))[1]
mode_val

hist(score,main="Histogram of scores",xlab="Score",ylab="Freq",col="skyblue",border="black")


f<-function(x,mu,beta){
  exp(-(x-mu)/beta)/(1+exp(-(x-mu)/beta))^2
}
int<-integrate(function(x) f(x,0,2),0,Inf)
int$value

int2<-integrate(function(x) f(x,0,4),0,Inf)
int2$value


r_func<-function(mu,beta,r){
  int<-function(x){
    int=(x^r)*f(x,mu,beta)
    int
  }
  moment<-integrate(int,0,Inf)$value
  moment
}

m1<-r_func(0,2,1)
m1

m2<-r_func(0,2,2)
m2

std_dev<-sqrt(m2-m1^2)
std_dev