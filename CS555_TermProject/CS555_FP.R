library(corrgram)

wine <- read.csv("winequality-red.csv")

#The 12th dimension, quality, is the variable we're trying to predict.
summary(wine$quality)
table(wine$quality)

#Try to use all the other variables instead of quality as input and do a linear regression
linear_quality = lm(quality ~ fixed.acidity+volatile.acidity+citric.acid+residual.sugar
                    +chlorides+free.sulfur.dioxide+total.sulfur.dioxide
                    +density+pH+sulphates+alcohol, data=wine)
summary(linear_quality)

#Visualizing variable relationships
corrgram(wine, lower.panel=panel.shade, upper.panel=panel.ellipse)


#Try Single-variable linear regression with the strongest correlation: alcohol
linear_quality_1 = lm(quality ~ alcohol, data = wine)
summary(linear_quality_1)

#Try Four-variable linear regression, using the variables: 
#alcohol, volatile.acidity, citric.acid, and sulphates
linear_quality_4 = lm(quality ~ alcohol + volatile.acidity + citric.acid 
                      + sulphates, data = wine)
summary(linear_quality_4)

#Visualizing the fits
linear_quality.res = resid(linear_quality)
linear_quality_1.res = resid(linear_quality_1)
linear_quality_4.res = resid(linear_quality_4)
plot(wine$alcohol, linear_quality.res)
points(wine$alcohol, linear_quality_1.res, col="red")
points(wine$alcohol, linear_quality_4.res, col="blue")

#Try to generalized linear model
glm_quality_1 = glm(quality~alcohol, data=wine, family=gaussian(link="identity"))
summary(glm_quality_1)
plot(glm_quality_1)
glm_quality_2 = glm(quality~alcohol, data=wine, family=gaussian(link="log"))
summary(glm_quality_2)
plot(glm_quality_2)
glm_quality_3 = glm(quality~alcohol+sulphates,data=wine,family=poisson(link="identity"))
summary(glm_quality_3)
plot(glm_quality_3)

#residuals
glm_quality_1.res = resid(glm_quality_1)
glm_quality_2.res = resid(glm_quality_2)
glm_quality_3.res = resid(glm_quality_3)

plot(wine$alcohol, glm_quality_1.res) # plot residuals against alcohol variable
points(wine$alcohol, glm_quality_2.res, col="red")
points(wine$alcohol, glm_quality_3.res, col="blue")

plot(wine$alcohol,wine$quality)
points(wine$alcohol,predict(glm_quality_3,wine),col="blue")
points(wine$alcohol,predict(glm_quality_1,wine),col="red")

#Classification
wine$bad <- wine$quality <= 4
wine$okay <- wine$quality == 5 | wine$quality == 6
wine$good <- wine$quality >= 7
head(wine)
summary(wine)