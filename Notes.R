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



#Risiduals

cars_lm <- lm(dist ~ speed, data = cars)
plot(dist ~ speed, data = cars)
abline(cars_lm)
summary(cars_lm)

round(mean(cars_lm$residuals),5)
hist(cars_lm$residuals)

#Variance numbers are off
var(cars_lm$residuals)
sd(cars_lm$residuals)

#Right numbers for MSE/variance
anova(cars_lm)

semistud <- cars_lm$residuals / summary(cars_lm)[[6]]
plot(semistud ~ cars_lm$fitted.values)
plot(cars_lm, which = 1)

plot(cars_lm)
plot(cars_lm$residuals, type = "b")
plot(cars_lm$residuals, type = "o")
boxplot(cars_lm$residuals, horizontal = TRUE, boxwex = 0.5)


library(mosaic)
library(alr3)
library(lmtest)
#BPTEST
#bptest(mylm, studentize = FALSE) 
#myqq <- qqnorm(mylm$residuals)
#cor(myqq$x, myqq$y)

#pureErrorAnova(mylm) p-value from the lack of fit line
plot(dist ~ speed, data = cars)
#regular plot
plot(log(dist)~speed, data = cars)
#transformed the x
plot(sqrt(dist)~speed, data = cars)
#transformed the y
plot(dist ~ I(speed^2), data = cars)
#transformed both x and y
plot(sqrt(dist) ~ I(speed^2), data = cars)

#regular plot
plot(height ~ age, data = Loblolly)

#practice
plot(log(height)~ age, data = Loblolly)
plot(sqrt(height)~ age, data = Loblolly)
plot(height ~ log(age), data = Loblolly)

plot(sqrt(height) ~ log(age), data = Loblolly)
lob_sqrt_lm <- lm(height ~ sqrt(age), data = Loblolly)
lob_x_sqrt <- lm(sqrt(height)~log(age), data = Loblolly)
pureErrorAnova(lob_x_sqrt)


#regular plot
ora_lm <- lm(circumference~ age, data = Orange)
plot(circumference ~ age, data = Orange)


#practice
plot(circumference ~ sqrt(age), data = Orange)
boxCox(ora_lm)
plot(height^(5/4) ~ age, data = Loblolly)
lob_crazy <- lm(height^(5/4) ~ age, data = Loblolly)
pureErrorAnova(lob_crazy)

plot(lob_crazy, which = 1)

#steps to take
plot(dist ~ speed, data = cars)
cars_lm <- lm(dist ~ speed, data = cars)
abline(cars_lm)
plot(cars_lm, which  = 1)
plot(sqrt(dist) ~ speed, data = cars)
cars_lm_sqrty <- lm(sqrt(dist) ~ speed, data = cars)
abline(cars_lm_sqrty)
plot(dist ~ speed, data = cars)
coef(cars_lm_sqrty)
curve( (1.2771 + 0.3224*x)^2, add = TRUE)
predict(cars_lm_sqrty, data.frame(speed = 20), interval = "prediction")
abline(h=predict(cars_lm_sqrty, data.frame(speed=20), interval = "prediction"), lty = 2)
abline(v=20, lty=2, col="gray")
abline(v=20, lty=2, col="gray")
abline(h=predict(cars_lm_sqrty, data.frame(speed=20), interval = "prediction")^2, lty = 2)
predict(cars_lm_sqrty, data.frame(speed=20), interval = "prediction")^2


plot(dist ~ speed, data = cars)
lines(lowess(cars$speed, cars$dist, f = .05))
cars_lm <- lm(dist~speed, data = cars)
abline(cars_lm)
plot(cars_lm, which = 1)

#powerful, robust
cars %>% 
  ggplot(aes(x = speed, y = dist))+
  geom_point()+
  geom_smooth()

plot(dist~speed, data = cars)
lines(lowess(cars$speed, cars$dist, f = .45))
abline(cars_lm)


dat <- read.table("http://users.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Chapter%20%203%20Data%20Sets/CH03TA10.txt")
colnames(dat) <- c("alpha_count", "plutonium")

plot(alpha_count ~ plutonium, data = dat[-24,])
lines(lowess(dat$plutonium,dat$alpha_count))
dat %>% 
  ggplot(aes(x = plutonium, y = alpha_count)) +
  geom_point()+
  geom_smooth(method = "lm")+
  geom_smooth(method="loess", se = FALSE)
  
dat_lm <- lm(alpha_count~plutonium, data = dat[-24,])
#par(mfrow=c(1,2))
plot(dat_lm, which = 1:2)
pureErrorAnova(dat_lm)
plot(sqrt(alpha_count)~sqrt(plutonium), data = dat[-24,])
bptest(dat_lm)

lmt <- lm(sqrt(alpha_count)~sqrt(plutonium), data = dat[-24,])
bptest(lmt)
pureErrorAnova(lmt)


plot(dist ~ speed, data = cars)
pairs(airquality)
plot(Ozone ~ Temp, data = airquality)
air_lm <- lm(Ozone ~ Temp, data = airquality)
abline(air_lm)
boxCox(air_lm)
boxCox(air_lm, lambda = c(0,.25,.5,1))
plot((Ozone^0.25)~Temp, data = airquality)
plot(log(Ozone)~ Temp, data = airquality)
air_lmt <- lm(I(Ozone^.25)~Temp, data = airquality)
abline(air_lmt)
plot(air_lmt, which = 1:2)
plot(I(Ozone^0.25)~ I(Temp^2), data = airquality)
air_lmt2 <- lm(I(Ozone^0.25)~ I(Temp^2), data = airquality)
plot(air_lmt2, which = 1:2)
plot(Ozone ~ Temp, data = airquality)



plot(tip~total_bill, data = tips)
tops_lm <- lm(tip~total_bill, data = tips)
summary(tops_lm)
abline(tops_lm)
plot(log(tip)~(total_bill), data = tips)
predict(tops_lm, data.frame(total_bill = 30), interval = "prediction")
2*pt(abs((0.105025-0.15)/0.007365), 242,lower.tail = FALSE)


#t = b1-B10/S{b1}
#H0= B1 = 0.15
# t  = (0.105-0.15)/0.007365

plot(height~age, data = Loblolly)
lob_lm2 <- lm(height~age +I(age^2), data = Loblolly)
summary(lob_lm)
curve(-7.607323 + 3.9859*x - 0.0498*x^2, add = TRUE)
lob_lm2 <- lm(height~age +I(age^2)+I(age^3), data = Loblolly)
b <- coef(lob_lm2)
curve(b[1]+b[2]*x+b[3]*x^2+b[4]^3, add = TRUE, col="red")
summary(lob_lm2)
cor(Loblolly$age-mean(Loblolly$age), (Loblolly$age-mean(Loblolly$age))^2)




pairs(Utilities)
ggpairs(Utilities)
u2 <- Utilities
colnames(u2)
colnames(u2) <- c("X1","X2","X3","X4","X5","X6","X7","X8","X9","X9","X10","X11","X12")

par(mfrow=c(1,2))
plot(totalbill ~ gasbill, data = Utilities)
plot(totalbill ~ temp, data = Utilities)
u_gas <- lm(totalbill ~ gasbill, data = Utilities)
summary(u_gas)

#golf rules, lowest the best
AIC(u_gas)

X1 <- runif(117, 4, 28)
X2 <- runif(117, 55, 120)
X3 <- runif(117, 1, 12)

Y <- 16 + 7*X2 - 13*X2^2 + rnorm(117, 0, 1.2)
mydata <- data.frame(Y = Y,
                     X1 = X1,
                     X2 = X2,
                     X3 = X3)
pairs(mydata)

lm1 <- lm(Y ~ X1, data = mydata)
lm2 <- lm(Y~X3, data = mydata)
lm3 <- lm(Y~X2, data = mydata)
summary(lm1)
summary(lm2)
summary(lm3)
AIC(lm1)
AIC(lm2)
AIC(lm3)
lm4 <- lm(Y ~ X2 + I(X2^2), data = mydata)
summary(lm4)
lm1 <- totalbill ~ month + I(month^2), data = Utilities)
plot(lm1$residuals ~ year, data = Utilities)
lm2 <- lm(totalbill ~ month + I(month^2) + year, data = Utilities)
summary(lm2)


added variable plots
if the Xnew and Residuals plot has a nice pattern we will add Xnew to our model


plot(dist ~ speed, data = cars)
lm1 <- lm(dist ~ speed, data = cars)
abline(lm1)

cars %>% 
  ggplot(aes(x = speed, y = dist))+
  geom_point()+
  geom_smooth(method = "lm")+
  geom_smooth(method = "loess", se = FALSE)

lm2 <- lm(dist ~ speed + I(speed^2), data = cars)
b <- lm2$coefficients

cars %>% 
  ggplot(aes(x = speed, y = dist))+
  geom_point()+
  geom_smooth(method = "lm")+
  geom_smooth(method = "loess", se = FALSE)+
  stat_function(fun = function(x) b[1] + b[2]*x + b[3]*x^2)

#rlm is in MASS
lm3 <- rlm(dist ~ speed, data = cars)
plot(dist ~ speed, data = cars)
abline(lm1)
abline(lm3, col = "red")


# robust quadratic don't listen to outliers listen to the main set of data

lm4 <- rlm(dist ~ speed + I(speed^2), data = cars)
b2 <- lm4$coefficients
cars %>% 
  ggplot(aes(x = speed, y = dist))+
  geom_point()+
  stat_function(fun = function(x) b[1] + b[2]*x + b[3]*x^2)+
  stat_function(fun = function(x) b2[1] + b2[2]*x + b2[3]*x^2,
                color = "red")


#regression trees
#library(mosiac)
plot(births ~ dayofyear, data = Births78)

b78 <- Births78
b78$wend <- as.factor(ifelse(b78$wday %in% c("Sat", "Sun"), "Weekend", 
                             "Weekday"))

tree <- ctree(births ~ dayofyear + wend, data = b78)
plot(tree)
mymeanswend <- mean(births ~ wend, data = subset(b78, wend == "Weekend" & dayofyear <= 185))





abline(h = mymeans, col = c("skyblue", "firebrick"))


#logistic regression
##bad idea
plot(Fail > 0 ~ Temp, data = challeng)
lm1 <- lm(Fail > 0 ~ Temp, data = challeng)
abline(lm1)

## good idea
plot(Fail > 0 ~ Temp, data = challeng)
glm1 <- glm(Fail > 0 ~ Temp, data = challeng, family = binomial())
b <- coef(glm1)
curve(exp(b[1] + b[2]*x)/(1+exp(b[1]+b[2]*x)), add = TRUE)

predict(glm1, data.frame(Temp = c(31,60)), type = "response")
#predict(glm1, data.frame(Temp = 60), type = "response")
summary(glm1)



challeng %>% 
  ggplot(aes(x = Temp, y = as.numeric(Fail > 0)))+
  geom_point()+
  geom_smooth()+
  stat_smooth(method = "glm", method.args = list(family= "binomial"))

# PIEi = probability of success piei / 1-piei
#probabililty = 3/10
#Odds = 3/7
              
curve(x/(1-x), from = 0, to = 1)
abline(abline(v = 1, lty = 2))
curve(log(x/(1-x)), from = 0, to = 1, add = TRUE, col = "green")

#log(ODDS) = B0 + B1Xi


#Find the p-value

carslm <- lm(dist ~ speed, data = cars)
summary(carslm)

t = (3.9324 - 0)/0.4155
2*pt(abs(t),48, lower.tail = FALSE)
confint(carslm)

predict(carslm, data.frame(speed = 12), interval = "prediction")
predict(carslm, data.frame(speed = 12), interval = "confidence")
