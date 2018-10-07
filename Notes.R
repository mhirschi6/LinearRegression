points = Yi = B0 + B1 Xi + Ei
where Ei ~ N(O,O^2)

B0 + B1 Xi = slope
Ei = error term
Ei ~N(O,O^2) = statistical relationship

YiHat = b0 +bi*Xi the estimated line
E{Y} = Bo + Bi*X the true line 
ei = Yi - Y^i

xi <- c(7,15,4,2,9)
beta0 <- 2.3
beta1 <- 3.14
epsiloni <- rnorm(5, 0, 1.2)

yi = beta0 + beta1*xi + epsiloni

mydate <- data.frame(x = xi, y = yi)


xi <- c(7,2,4,11,15)
beta0 <- 9
beta1 <- 4.3
epsiloni <- rnorm(5, 0, 1.2)

yi <- beta0 +beta1 * xi + epsiloni

mydata <- data.frame(y=yi, x = xi)
View(mydata)
mylm <- lm(y ~x, data = mydata)
plot(y ~ x, data = mydata)
abline(mylm)
summary(mylm)
View(cbind(mydata, Yhat=mylm$fitted.values))
sum(mylm$residuals^2)
sum(mylm$residuals^2)/3
sqrt(sum(mylm$residuals^2)/3)
sum((mydata$x - mean(mydata$x))^2) 
var(mydata$x)*4    #shortcut to code one line above


standard error is the wiggle in slope

b1 ~ N(Beta1, sigma^2 {b1})

sigma^2{b1} = 
  
  MSE = Mean Squared Errors
SSE = Sum of Squared Errors

ei = yi - yhati    residual 


plot(dist ~ speed, data = cars)

mylm <- lm(dist~speed, data = cars)
abline(mylm)
predict(mylm, data.frame(speed = 15))
coef(mylm)
summary(mylm)



xbar = summation i = 1 to n  xi / n

#sum( ()*() )/sum( ()^2)
    xi <- cars$speed
    yi <- cars$dist

    
sum((xi - mean(xi)) * (yi - mean(yi)))/sum((xi - mean(xi))^2)


SSE - sum squared errors (residuals)
SSR - sum of squares regression
SSTO - igrnores x SSTO = SSE + SSR


cars_lm <- lm(dist~speed, data=cars)
summary(cars_lm)
plot(dist~speed, data=cars)
abline(cars_lm)
abline(h=mean(cars$dist))
anova(cars_lm)

SSTO <- sum((cars$dist- mean(cars$dist))^2)
SSE <- sum((cars_lm$residuals)^2)
SSR <- sum((cars_lm$fitted.values - mean(cars$dist))^2)
# sum sq residuals SSE Mean sq residuals MSE
# residual standard error sqrt MSE

full <- lm(dist~speed, data = cars)
summary(full)

#intercept only model
reduced <- lm(dist~1, data=cars)
summary(reduced)

anova(reduced,full)
#RSS = SSE

anova(full)


E{Y} Confidence band
E{Yi}Confidence interval for E{Yi} predeict (,interval = "confidence")
BEta0 Confidence interval confint(mylm)
Beta1 Ditto
Epsilon1 
MSE = SSE / N - 2 
sigma ^ 2 

predict(my_lm, intveral = "predict")


