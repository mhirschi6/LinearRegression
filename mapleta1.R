library(tidyverse)
library(mosaic)
library(car)


model <- lm(length ~ width + I(width^2), data = KidsFeet)
summary(model)
plot(length ~ I(width/1), data = KidsFeet)
b <- model$coefficients
curve(b[1]+b[2]*x +b[3]*x^2, add = TRUE, col = "blue")


model2 <- lm(length ~ width + sex, data = KidsFeet)
summary(model2)
plot(length ~ width, data = KidsFeet, col = as.factor(sex))
a <- model2$coefficients
curve(a[1] + a[2]*x, add = TRUE, col = "blue")
curve((a[1] + a[3]) + a[2]*x, add = TRUE, col = "red")


model3 <- lm(length ~ width + I(width^2) + sex + I(width^2):sex, 
             data = KidsFeet)      
summary(model3)

plot(length ~ width, col = as.factor(sex), data = KidsFeet)
b <- model3$coefficients
curve(b[1] + b[2] *x + b[3]*x^2, add = TRUE, col = "blue")
curve(b[1] + b[4] + b[2]*x + (b[3]+ b[5])*x^2, add = TRUE, col = "red")
      
      