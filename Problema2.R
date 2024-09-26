#Codigo para problema 2

iris
mis_dades <- iris;
y <- mis_dades$Sepal.Length
y

x <- mis_dades$Petal.Length
plot(x,y)

xbar <- mean(x)
ybar <- mean(y)

m <- (sum((x-xbar)*(y-ybar)))/sum((x-xbar)^2)
m

b <- ybar - (m*xbar)
b

a = (m*1.5) + b
a

mod <- lm(y~x)
mod

ypredicted <- predict(mod,data.frame(x=x))
ypredicted

plot(x,y)
lines(x,ypredicted)


Rsq <- sum((ypredicted-ybar)^2)/sum((y-ybar)^2)
Rsq

summry(mod)
sqrt(Rsq)
cor.test(x,y) #te da e valor de correlacion de tu funcion igual

