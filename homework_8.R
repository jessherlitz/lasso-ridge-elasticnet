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