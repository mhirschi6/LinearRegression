air.lm <- lm(Solar.R ~ Wind, data=airquality)
summary(air.lm)

X = c(2,3,8,9,11)
Y = c(3.2,5.1,6.8,8.1,9.2)

my_lm <- lm(Y~X)
predict(my_lm, data.frame(X=5), interval = "predict")

cars_lm <- lm(dist ~ speed, data = cars)
summary(cars_lm)

summary(lm(weight ~ height, data=Davis))

height <- lm(weight~height, data = Davis)
plot(height)


iq <- lm(IQbio ~ IQfoster, data = Burt)
summary(iq)

age <- lm(exercise ~ age, data = Blackmore)
summary(age)

plot(age)

plot(rate ~ slim, data=Highway1, xlab="Speed Limit", ylab="Accident Rate per Million Vehicle Miles", main="1973 Minnesota Highway Accident Records")

rate <- lm(I(rate^2)) ~ slim, data = Highway1)
summary(rate)

cars.lm <- lm(speed ~ dist, data=cars)
summary(cars.lm)
predict(cars.lm, data.frame(dist = 20), interval = "predict")

utli <- lm(kwh ~ ccf, data = Utilities)

summary(utli)
predict(utli, data.frame(ccf = 1), interval = "predict")
predict(utli, interval = "predict")


width <- lm(width ~ length, data = KidsFeet)
predict(width, data.frame(length = 25),interval = "predict" )


tree <- lm(circumference ~ age, data = Orange)
boxCox(tree)
tree$coefficients
plot(tree)

plot(age~ circumference, data = Orange)
abline(tree)
