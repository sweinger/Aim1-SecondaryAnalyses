# Regression Program
## model population: all ARI visits in 12 month period

#install packages? 
install.packages("lme4")

#consults_analytical is a data frame of the analytical file created with SQL. provider_id and other strings are factors, rest are continuous variables or binary flags

# Specify a null model with no predictors
null_model <- glmer(abx_inappropriate ~ (1 | provider_id), data = consults_analytical, family = "binomial")

# Specify the full model using all of the potential predictors
# Need to finalize structure/presence of: comorbidities, high-freq provider categories, time of day categories
full_model <- glmer(abx_inappropriate ~ (1 | provider_id) + patient_sat + patient_gender + patient_age + patient_state + patient_problemflag + patient_totalconsults + patient_group_type + patient_client_segment + provider_gender + provider_specialty + provider_age + provider_teladoc_tenure + provider_poorsat + provider_highfreq + provider_median_duration + consult_region + consult_method + consult_timeofday + consult_waittime + consult_duration + consult_dx + consult_season + consult_month + consult_fluseason + consult_visitno_sincelogin, data = consults_analytical, family = "binomial")
summary(full_model)

se <- sqrt(diag(vcov(full_model)))
# table of estimates with 95% CI
(table_full_model <- cbind(Est = fixef(full_model), LL = fixef(full_model) - 1.96 * se, UL = fixef(full_model) + 1.96 * se))
# odds ratio?
exp(table_full_model)

# Use a forward stepwise algorithm to build a parsimonious model
step_model <- step(null_model, scope = list(lower = null_model, upper = full_model), direction = "forward")
summary(step_model)
se <- sqrt(diag(vcov(step_model)))
# table of estimates with 95% CI
(table_step_model <- cbind(Est = fixef(step_model), LL = fixef(full_modstep_modelel) - 1.96 * se, UL = fixef(step_model) + 1.96 * se))
# odds ratio?
exp(table_step_model)

# Estimate the stepwise prescription probability
consults_analytical_pred<-data.frame(consults_analytical)
step_model_pred <- predict(step_model, consults_analytical_pred, type = "response")

# Plot the ROC (actual vs predicted) of the stepwise model
library(pROC)
ROC <- roc(consults_analytical$abx_inappropriate, step_model_pred)
plot(ROC, col = "red")
auc(ROC)
