## base model

model1 <-glm(abx_inappropriate ~ phys_base_abxrate
# Specify a null model with no predictors
null_model <- glm(abx_inappropriate ~ 1, data = consults_analytical, family = "binomial")

# Specify the full model using all of the potential predictors
full_model <- glm(abx_inappropriate ~ ., data = consults_analytical, family = "binomial")
summary(full_model)
exp(cbind(OR = coef(full_model), confint(full_model)))

# Use a forward stepwise algorithm to build a parsimonious model
step_model <- step(null_model, scope = list(lower = null_model, upper = full_model), direction = "forward")
summary(step_model)
exp(cbind(OR = coef(step_model), confint(step_model)))

# Estimate the stepwise prescription probability
step_prob <- predict(step_model, type = "response")

# Plot the ROC (actual vs predicted) of the stepwise model
library(pROC)
ROC <- roc(consults_analytical$abx_inappropriate, step_prob)
plot(ROC, col = "red")
auc(ROC)