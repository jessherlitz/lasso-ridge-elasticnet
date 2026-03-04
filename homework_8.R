### Homework 8

library(glmnet)

## Question 11.1.1

# loading data
crime_data <- read.table(
  "uscrime.txt", 
  header = TRUE
)

#View(crime_data)

regression_model <- lm(Crime ~ ., data = crime_data)
summary(regression_model)

stepwise_regression_model <- step(regression_model, 
                                  direction = "both", 
                                  trace = TRUE)

summary(stepwise_regression_model)

stepwise_bic <- step(regression_model, 
                     direction = "both", 
                     k = log(nrow(crime_data)),
                     trace = TRUE)
summary(stepwise_bic)


## Question 11.1.2

scaled_predictors <- as.matrix(scale(crime_data[, 1:15]))
response <- crime_data[, 16]

set.seed(111)
lasso_cross_validation <- cv.glmnet(scaled_predictors, response, alpha = 1)

lasso_cross_validation$lambda.min
lasso_cross_validation$lambda.1se

coef(lasso_cross_validation, s = "lambda.min")
coef(lasso_cross_validation, s = "lambda.1se")
plot(lasso_cross_validation)

cbind(lambda = lasso_cross_validation$lambda,
      mean_error = lasso_cross_validation$cvm,
      std_error = lasso_cross_validation$cvsd)

colMeans(crime_data[, 1:15])
apply(crime_data[, 1:15], 2, sd)

## Question 11.1.3

alphas <- seq(0, 1, 0.1)
print(alphas)

set.seed(111)
cv_errors <- c()

for (a in alphas) {
  model <- cv.glmnet(
    scaled_predictors, 
    response, 
    alpha = a
  )
  cv_errors <- c(cv_errors, min(model$cvm))
}
print(cv_errors)

# finding the alpha
cbind(alpha = alphas, minimum_cv_error = cv_errors)
best_alpha <- alphas[which.min(cv_errors)]
print(best_alpha)

set.seed(111)
best_model <- cv.glmnet(
  scaled_predictors, 
  response, 
  alpha = best_alpha
)

coef(best_model, s = "lambda.min")
coef(best_model, s = "lambda.1se")
best_model$lambda.min
best_model$lambda.1se

### plotting
plot(best_model)

# testing new data
test_data <- data.frame(
  M = 14.0, So = 0, Ed = 10.0, Po1 = 12.0, Po2 = 15.5,
  LF = 0.640, M.F = 94.0, Pop = 150, NW = 1.1, U1 = 0.120,
  U2 = 3.6, Wealth = 3200, Ineq = 20.1, Prob = 0.04, Time = 39.0
)

scaled_test <- scale(test_data, 
                     center = colMeans(crime_data[, 1:15]), 
                     scale = apply(crime_data[, 1:15], 2, sd))

scaled_test <- as.matrix(scaled_test)

## predicting using our test data
predict(best_model, s = best_model$lambda.min, newx = scaled_test)
predict(lasso_cross_validation, s = lasso_cross_validation$lambda.min, newx = scaled_test)










