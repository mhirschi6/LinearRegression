library(tidyverse)
library(mosaic)
library(car)
library(MASS)


# SLR model and robust model 
model <- lm(mpg ~ qsec, data = mtcars)


model1 <- rlm(mpg ~ qsec, data = mtcars)

plot(mpg ~ qsec, pch=21, bg="gray", col=rgb(1-model1$w, model1$w, 0),
     data = mtcars)
abline(model)
abline(model1)

#weight by hand don't do just silly
#lm1 <- lm(mpg ~ qsec, data = mtcars)
#MAD <- 1/0.6745*median(abs(lm1$res ~ median(lm1$res)))
#ui <- lm1$res/MAD

#Huber:
#w <- ifelse(abs(ui) <= 1.345, 1, 1.345/abs(ui))

# quad model and robust model

model2 <- lm(mpg ~ wt + I(wt^2), data = mtcars)

model3 <- rlm(mpg ~ wt + I(wt^2), data = mtcars)

b <- model2$coefficients
b2 <- model3$coefficients
mtcars %>% 
  ggplot(aes(x = wt, y = mpg))+
  geom_point()+
  stat_function(fun = function(x) b[1] + b[2]*x + b[3]*x^2)+
  stat_function(fun = function(x) b2[1] + b2[2]*x + b2[3]*x^2,
                color = "red")

#library(party)
ctree <- ctree(circumference ~ age, data = Orange)
unique(predict(ctree))
model6 <- lm(circumference ~ age, data = Orange)
plot(ctree, type = "simple")
palette(c("green", "red","blue"))
plot(circumference ~ age, col = as.factor(predict(ctree)), data = Orange)
abline(model6)
lines(c(0,663), rep(60.667,2), col = "skyblue")
lines(c(664,1230), rep(139.90000,2), col = "red")
lines(c(1231, 1582), rep(174.6,2), col = "green")
