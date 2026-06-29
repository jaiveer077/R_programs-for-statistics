head(mtcars)
plot(mtcars$hp,mtcars$mpg,col="red",xlab="HorsePower",ylab="Miles")
mean(mtcars$mpg[mtcars$cyl==6])