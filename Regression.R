# dataset
cgood <- read.csv("D:/sem 6/Data Analytics/ConsumerGood.csv")
head(cgood)

nrow(cgood)
ncol(cgood)

#Spliting the data
# install.packages('caTools')
library(caTools)

set.seed(123)
split <- sample.split(cgood$price, SplitRatio = 0.7)


split


train_cgood <- subset(cgood, split == TRUE)
test_cgood <- subset(cgood, split == FALSE)

head(train_cgood)
head(test_cgood)

nrow(train_cgood)
nrow(test_cgood)


# ------------------------------------------ #


# Linear Regression

rel_dist <- lm(price ~ distribution, data = train_cgood) 
rel_dist
summary(rel_dist)
plot(train_cgood$distribution, train_cgood$price)
abline(rel_dist)

rel_share <- lm(price ~ share, data = train_cgood) 
rel_share
summary(rel_share)
plot(train_cgood$share, train_cgood$price)
abline(rel_share)

# Prediction based on linear model
new.dist <- data.frame(distribution = test_cgood$distribution)
pred_dist <- predict(rel_dist, new.dist)
pred_dist
summary(pred_dist)

new.share <- data.frame(share = test_cgood$share)
pred_share <-predict(rel_share, new.share)
pred_share

# Adding predicted value of price
# to test dataset for comparison
my_test <- data.frame(test_cgood, pred_dist, pred_share)
my_test
nrow(my_test)


# Calc Cost Function
m <- nrow(test_cgood)
cost_dist <- (1/(2*m))*sum((my_test$price - my_test$pred_dist)^2)
cost_dist

cost_share <- (1/(2*m))*sum((my_test$price - my_test$pred_share)^2)
cost_share


# calc sum of squares
actual <- my_test$price
predicted <- my_test$pred_dist
sse <- sum((actual - predicted)^2)
sse
sst <- sum((actual - mean(actual))^2)
sst

# Coefficient of Determination
Rsq <- 1 - (sse/sst)
Rsq







plot( my_test$distribution, my_test$price, col="red" )
par(new=TRUE)
plot( my_test$distribution, my_test$pred_dist, col="green" )


# ---------------------------------------------------- #



# Multiple Regression

multi_model <- lm(price ~ distribution + share, data = train_cgood)
multi_model
summary(multi_model)
plot(train_cgood$distribution + train_cgood$share, train_cgood$price)


# Prediction based on Multiple Regression

new.multi <- data.frame(
  distribution = test_cgood$distribution, 
  share = test_cgood$share)

pred_multi <- predict(multi_model, new.multi)
pred_multi

# Adding predicted value of price
# to test dataset for comparison
my_test.multi <- data.frame(test_cgood, pred_multi)
my_test.multi
nrow(my_test.multi)

# calc cost function for Multiple Regression Model
cost_multi <- (1/(2*m))*
  sum((my_test.multi$price - my_test.multi$pred_multi)^2)
cost_multi



# -----------------------------Curvilinear Regression---------------------------- #


# y = a + bx + cx^2

curvi_model <- nls(
  price ~ a + b * I(distribution^z), 
  data = train_cgood, 
  start = list(a = 1, b = 1, z = 2))
curvi_model
summary(curvi_model)

# plot the curvilinear model
plot(train_cgood$distribution, train_cgood$price)
lines(train_cgood$distribution, predict(curvi_model))

# Prediction based on Curvilinear Regression
new.curvi <- data.frame(distribution = test_cgood$distribution)
pred_curvi <- predict(curvi_model, new.curvi)

# Adding predicted value of price
# to test dataset for comparison

my_test.curvi <- data.frame(test_cgood, pred_curvi)
my_test.curvi
nrow(my_test.curvi)

# calc cost function for both models
cost_curvi <- (1/(2*m))*
  sum((my_test.curvi$price - my_test.curvi$pred_curvi)^2)
cost_curvi



# -------------------------------Power Curve-------------------------------- #


# y = a * x^b

power_model <- nls(
  price ~ a * (distribution ^ b), 
  data = train_cgood, 
  start = list(a = 1, b = 0.2))
power_model
summary(power_model)

# plot the Power Curve model
plot(train_cgood$distribution, train_cgood$price)
lines(train_cgood$distribution, predict(power_model))

# Prediction based on Power Curve Model
new.power <- data.frame(distribution = test_cgood$distribution)
pred_power <- predict(power_model, new.power)

# Adding predicted value of price
# to test dataset for comparison

my_test.power <- data.frame(test_cgood, pred_power)
my_test.power
nrow(my_test.power)

# calc cost function for power curve model
cost_power <- (1/(2*m))*
  sum((my_test.power$price - my_test.power$pred_power)^2)
cost_power



# --------------------------------Ridge Regression------------------------------ #


# install.packages("glmnet")
library(glmnet)


# creating a general model and check for best value of lambda
ridge_model <- glmnet(train_cgood, train_cgood$price, alpha = 0)
plot(ridge_model,xvar="lambda",label=TRUE)

# selecting best lambda
bestlam_r <- cv.glmnet(
  as.matrix(train_cgood), 
  train_cgood$price, alpha = 0)$lambda.min

# recreating the model according to best lambda
ridge_model <- glmnet(
  train_cgood, train_cgood$price, 
  alpha = 0, lambda = bestlam_r)
coef(ridge_model)

# Predicting price
pred_ridge <- predict(
  ridge_model, s = bestlam_r, 
  newx = as.matrix(test_cgood))
head(pred_ridge)

# Adding predicted value of price
# to test dataset for comparison

my_test.ridge <- data.frame(test_cgood, pred_ridge)
colnames(my_test.ridge)[5] <- "pred_ridge"
my_test.ridge
nrow(my_test.ridge)

# calc cost function for ridge regression
cost_ridge <- (1/(2*m))*
  sum((my_test.ridge$price - my_test.ridge$pred_ridge)^2)
cost_ridge

# calc Coeff. of Determination
sse <- sum((my_test.ridge$price - my_test.ridge$pred_ridge)^2)
sst <- sum((my_test.ridge$price - mean(my_test.ridge$price))^2)
Rsq <- 1 - sse/sst
Rsq



# ---------------------------Lasso Regression------------------------------- #



# creating a general model and check for best value of lambda
lasso_model <- glmnet(train_cgood, train_cgood$price, alpha = 1)
plot(lasso_model,xvar="lambda",label=TRUE)

# selecting best lambda
bestlam_l <- cv.glmnet(
  as.matrix(train_cgood), 
  train_cgood$price, alpha = 1)$lambda.min
bestlam_l

# recreating the model according to best lambda
lasso_model <- glmnet(
  train_cgood, train_cgood$price, 
  alpha = 1, lambda = bestlam_l)
coef(lasso_model)

# Predicting price based on Lasso Regression
pred_lasso <- predict(
  lasso_model, s = bestlam_l, 
  newx = as.matrix(test_cgood))
head(pred_lasso)

# Adding predicted value of price
# to test dataset for comparison

my_test.lasso <- data.frame(test_cgood, pred_lasso)
colnames(my_test.lasso)[5] <- "pred_lasso"
my_test.lasso
nrow(my_test.lasso)

# calc cost function for lasso model
cost_lasso <- (1/(2*m))*
  sum((my_test.lasso$price - my_test.lasso$pred_lasso)^2)
cost_lasso

# calc Coeff. of Determination
sse <- sum((my_test.lasso$price - my_test.lasso$pred_lasso)^2)
sst <- sum((my_test.lasso$price - mean(my_test.lasso$price))^2)
Rsq <- 1 - sse/sst
Rsq



# ---------------------Linear Regression with Gradient Descent------------------------- #


GradientDescent <- function(x, y)
{
  # learning rate
  r <- 0.2
  
  # initialize m and b with random values
  m <- 1
  b <- 2
  
  predicted <- m * x + b
  for(i in range(1:100))
  {
    # Compute the gradient
    sse <- (1 / 2) * sum((y - predicted) ^ 2)
    
    f <-function(m, b) (1 / 2) * ((y - (m * x + b)) ^ 2)
    deriv(~f(m, m ^ 2),"m")
    
    
    # f <- expression((1 / 2) * ((y - (m * x + b)) ^ 2))
    # Update coefficient in the direction of optimal m and b
    m <- m - r * deriv(f, m)
    b <- b - r * deriv(f, b)
    predicted <- m * x + b
    plot(x, predicted)
  }
}

GradientDescent(test_cgood$distribution, test_cgood$price)


# ---------------------------------------------------- #



# Effect of lambda on Ridge and Lasso curved slope
# + create own function
# + find loss function
# + determine loss function
# + detect appropriate lambda value





# -------------------------------Trial Section----------------------------------- #



# squared error cost function
cost <- function(X, y, theta) {
  sum( (X %*% theta - y)^2 ) / (2*length(y))
}
# learning rate and iteration limit
alpha <- 0.01
num_iters <- 33
# keep history
cost_history <- double(num_iters)
theta_history <- list(num_iters)
# initialize coefficients
theta <- matrix(c(0,0), nrow=2)
# add a column of 1's for the intercept coefficient
X <- cbind(1, as.matrix(train_cgood$distribution))
X
y <- as.matrix(train_cgood$price)
y
# gradient descent
for (i in 1:num_iters) {
  error <- (X %*% theta - y)
  delta <- t(X) %*% error / length(y)
  theta <- theta - alpha * delta
  cost_history[i] <- cost(X, y, theta)
  theta_history[[i]] <- theta
}

error
delta
theta
cost_history
theta_history


coef()

# growth <- read.csv("G:\\6th SEMESTER\\LAB FILES\\DA LAB\\datasets\\GrowthDJ.csv")

# #view the data
# head(growth)
# 
# # rows and cols
# nrow(growth)
# ncol(growth)
# 
# #data class
# data.class(growth$X)
# data.class(growth$oil)
# data.class(growth$inter)
# data.class(growth$oecd)
# data.class(growth$gdp60)
# data.class(growth$gdp85)
# data.class(growth$gdpgrowth)
# data.class(growth$popgrowth)
# data.class(growth$invest)
# data.class(growth$school)
# data.class(growth$literacy60)
# 




# 0. Build linear model 
data("cars", package = "datasets")
model <- lm(dist ~ speed, data = cars)
# 1. Add predictions 
pred.int <- predict(model, interval = "prediction")
mydata <- cbind(cars, pred.int)
# 2. Regression line + confidence intervals
library(ggplot2)
p <- ggplot(mydata, aes(speed, dist)) +
  geom_point() +
  stat_smooth(method = lm)
# 3. Add prediction intervals
p + geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y = upr), color = "red", linetype = "dashed")







stem_diameter <- c(15, 15, 16, 17, 19, 23, 23, 24, 24, 25, 25, 26, 27, 28, 29, 30, 30, 32, 32, 33, 34, 34, 35, 36, 36, 37, 38, 40, 41, 41, 42, 42, 46, 48, 48, 49, 51, 54, 55, 60)

total_biomass <- c(0.25, 0.65, 0.40, 0.40, 0.65, 0.60, 0.20, 0.60, 0.50, 0.35, 0.70, 0.65, 0.95, 0.60, 0.80, 0.90, 0.70, 1.15, 1.00, 1.70, 1.95, 1.15, 1.70, 1.25, 1.95, 1.20,
                   2.70, 2.00, 3.70, 2.35, 1.50, 2.50, 5.05, 4.10, 2.85, 2.15, 3.50, 4.80, 5.30, 2.95)

stem_data.df <- data.frame(stem_diameter, total_biomass)


model1 <- lm(total_biomass ~ stem_diameter, data = stem_data.df)
summary(model1)
plot(total_biomass ~ stem_diameter)
lines(stem_diameter, predict(model1))


m <- nls(total_biomass ~ a*(stem_diameter^b), stem_data.df, start = list(a = -1.8, b = 0.1)) # power formula: y = a*x^b
summary(m)
plot(total_biomass ~ stem_diameter)
lines(stem_diameter, fitted(m), col = "green")


fit1 <- lm(log(total_biomass) ~ log(stem_diameter), data = stem_data.df)

#you were missing a `*` and had bad starting values
m <- nls(total_biomass ~ a*(stem_diameter^b), stem_data.df, 
         start = list(a = exp(coef(fit1)[1]), b = coef(fit1)[2])) # power formula: y = a*x^b
summary(m)
plot(total_biomass ~ stem_diameter)
curve(predict(m, newdata = data.frame(stem_diameter = x)), add = TRUE)

data <- read.csv(file="data.csv")
plot(data$days, data$visitors, log="xy", cex=0.8)
model <- lm(log(data$visitors) ~ log(data$days))
points(data$days,
       round(exp(coef(model)[1] + coef(model)[2] * log(data$days))),
       col="red")

library(glmnet)
#ridge
ridge.mod <- glmnet(x, y, alpha = 0, lambda = lambda)
predict(ridge.mod, s = 0, exact = T, type = 'coefficients')[1:6,]


lasso.mod <- glmnet(x[train,], y[train], alpha = 1, lambda = lambda)
lasso.pred <- predict(lasso.mod, s = bestlam, newx = x[test,])

