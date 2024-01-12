rm(list = ls())
QUERY_START_TIME <- Sys.time()
options(scipen = 100);options(max.print = 10000)
library(readxl);library(dplyr);library(caret);library(ggplot2);library(lattice);
library(rpart);library(lubridate);library(Metrics);library(glmnet);library(Matrix);
library(caret);library(kableExtra);library(class);library(openxlsx);library(gmodels);
library(MASS);library(reshape2);library(tidyr);library(ggpubr);library(tibble);
library(stringr);library(sqldf);library(car);library(ggpubr);library(tidyverse);
library(stringr);library(data.table);library(readxl);library(factoextra);library(writexl);
library(cluster);library(corrplot);library(tidyverse);library(mapsapi);library(lmtest);
library(dbplyr);library(DBI);library(odbc);library(xgboost);library(gridExtra);
library(tidyverse);library(keras);library(arrow);library(tidymodels);library(doFuture);
library(vip);library(gdata);library(xml2);library(tensorflow);library(tseries);
library(zoo);library(forecast);library(tidyverse);library(tidymodels);library(modeltime);
library(timetk);library(moments);
# ################################################################################################################
# set.seed(123)
# 
# db_connection <- DBI::dbConnect(odbc::odbc(),
#                                 Driver="SNowflakeDSIIDriver",
#                                 Server="va71008.east-us-2.azure.snowflakecomputing.com",
#                                 Database="PIQ_ANALYTICS_WORKSPACE",
#                                 SCHEMA="DEV",
#                                 UID="Kyle.Chauvin@petiq.com",
#                                 role="DATAANALYST",
#                                 authenticator="externalbrowser",
#                                 WAREHOUSE="PIQWH_ANALYTICS")
# 
# main_data_source <- DBI::dbGetQuery(db_connection,
#                                     "SELECT * FROM KC_COMMUNITY_CLINIC_DATA")

# main_data <- main_data_source

main_data <- read_excel("C:/Users/kyle.chauvin/OneDrive - PetIQ, LLC/Excel/TEMP CC DATA MASTER.xlsx")
str(main_data)
hist(main_data$PPC)
skewness(main_data$PPC)
hist(sqrt(main_data$PPC))
skewness(sqrt(main_data$PPC))

quartiles <- quantile(main_data$PPC, probs=c(.25, .75), na.rm = TRUE)
IQR <- IQR(main_data$PPC,na.rm = TRUE)
Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR
main_data <- subset(main_data, main_data$PPC > Lower & main_data$PPC < Upper)

# data$PPC <- sqrt(data$PPC)
# hist(data$PPC)
# skewness(data$PPC)
summary(main_data)
data <- main_data

summary(data)
nrow(data)
ncol(data)
################################################################################
################################################################################
################################################################################
# FINDING BEST VARIABLES

str(data)
colnames(data)
data <- as.data.frame(data)
data <- data %>% mutate(across(where(is.character), as.factor))
data <- data %>% mutate(across(where(is.logical), as.factor))

names <- c("Variable","Coefficient","Adj R Sq.","P-Value")
sig_variable_df <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(sig_variable_df) <- names

overall_p_value <- function(test) {
  f <- summary(test)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}
nrow(data)

summary(data)
str(data[,c(4,5,8,15:17,25:38,41:121,123:283)])
colnames(data[,c(4,5,8,15:17,25:38,41:121,123:283)])

for (i in c(4,5,8,15:17,25:38,41:121,123:284)) {  
  # if(is.factor(data[,i])==TRUE) {next}
  # if(is.logical(data[,i])==TRUE) {next}
  test <- (lm(PPC~
                CLINIC_LOCATION_RETAIL_PARTNER+
                CLINIC_LOCATION_STATE+
                ALL_OWNERS_IN_5_MILES+
                # ACTIVE_CC_WITHIN_5_MILES+
                # ACTIVE_WC_WITHIN_5_MILES+
                (data[,i]), data = data))
  sig_variable_df[nrow(sig_variable_df)+1,] = c(colnames(data)[i],
                                                round(test$coefficients[2],digits=5),
                                                round(summary(test)$r.squared,digits = 5),
                                                round(overall_p_value(test),digits=5))}


sig_variable_df<- sig_variable_df %>% filter(`P-Value`<=.05)
sig_variable_df[order(sig_variable_df$Coefficient,decreasing = FALSE),]
sig_variable_df[order(sig_variable_df$`Adj R Sq.`,decreasing = FALSE),]



test_data <- data[,c(21,4,5,8,15:17,25:38,41:121,123:283)]
fit1 <- lm(PPC ~ ., test_data)
fit2 <- lm(PPC ~ 1, test_data)
stepAIC(fit2,direction="forward",scope=list(upper=fit1,lower=fit2))




model2 <- lm(PPC ~ CLINIC_LOCATION_RETAIL_PARTNER + CLINIC_LOCATION_STATE + 
               ALL_OWNERS_IN_5_MILES + ACTIVE_CC_WITHIN_5_MILES + ACTIVE_WC_WITHIN_5_MILES + 
               NEAREST_COLLEGE + ALL_OWNERS_IN_30_MILES + HEAT_WOOD + MILITARY_BASES_IN_20_MILES + 
               HAWAIIAN + ACTIVE_WC_WITHIN_15_MILES + ASIAN + AGE_5_TO_9 + 
               AVERAGE_HH_SIZE + ACTIVE_VETS_IN_5_MILES + HU_VACANT + `_2_RACES` + 
               ACTIVE_SAME_STORE_LOCATIONS_5_MILES,data=data)
summary(model2)

options(warn=-1)
plot(model2,1,)
plot(model2,2)
plot(model2,3)
plot(model2,4)
options(warn=0)

















# colnames(main_data)
# model <- lm(PPC~
#               CLINIC_LOCATION_RETAIL_PARTNER+
#               CLINIC_LOCATION_STATE+
#               ALL_OWNERS_IN_5_MILES+
#               ACTIVE_CC_WITHIN_5_MILES+
#               ACTIVE_WC_WITHIN_5_MILES
#               # NEAREST_COLLEGE
#             , data = data)
# summary(model)
# 
# options(warn=-1)
# plot(model,1,)
# plot(model,2)
# plot(model,3)
# plot(model,4)
# options(warn=0)
# 
# str(data)


tidy_split <- initial_split(data, prop = 7 / 10)
tidy_train <- training(tidy_split)
tidy_test <- testing(tidy_split)
tidy_kfolds <- vfold_cv(tidy_train, 5)

tidy_rec <- recipe(PPC~
                     CLINIC_LOCATION_RETAIL_PARTNER + CLINIC_LOCATION_STATE + 
                     ALL_OWNERS_IN_5_MILES + ACTIVE_CC_WITHIN_5_MILES + ACTIVE_WC_WITHIN_5_MILES + 
                     NEAREST_COLLEGE + ALL_OWNERS_IN_30_MILES + HEAT_WOOD + MILITARY_BASES_IN_20_MILES + 
                     HAWAIIAN + ACTIVE_WC_WITHIN_15_MILES + ASIAN + AGE_5_TO_9 + 
                     AVERAGE_HH_SIZE + ACTIVE_VETS_IN_5_MILES + HU_VACANT + `_2_RACES` + 
                     ACTIVE_SAME_STORE_LOCATIONS_5_MILES
                   ,data = tidy_train) %>%
  step_nzv(c(ALL_OWNERS_IN_5_MILES,ACTIVE_CC_WITHIN_5_MILES,ACTIVE_WC_WITHIN_5_MILES,NEAREST_COLLEGE,
             ALL_OWNERS_IN_30_MILES,HEAT_WOOD,MILITARY_BASES_IN_20_MILES,HAWAIIAN,ACTIVE_WC_WITHIN_15_MILES,
             ASIAN,AGE_5_TO_9,AVERAGE_HH_SIZE,ACTIVE_VETS_IN_5_MILES,HU_VACANT,`_2_RACES`,ACTIVE_SAME_STORE_LOCATIONS_5_MILES)) %>%
  step_dummy(c(CLINIC_LOCATION_RETAIL_PARTNER,CLINIC_LOCATION_STATE), -all_outcomes()) %>% 
  step_normalize(c(ALL_OWNERS_IN_5_MILES,ACTIVE_CC_WITHIN_5_MILES,ACTIVE_WC_WITHIN_5_MILES,NEAREST_COLLEGE,
                   ALL_OWNERS_IN_30_MILES,HEAT_WOOD,MILITARY_BASES_IN_20_MILES,HAWAIIAN,ACTIVE_WC_WITHIN_15_MILES,
                   ASIAN,AGE_5_TO_9,AVERAGE_HH_SIZE,ACTIVE_VETS_IN_5_MILES,HU_VACANT,`_2_RACES`,ACTIVE_SAME_STORE_LOCATIONS_5_MILES)) %>%
  prep()


split_all <- initial_split(data, prop = 7 / 10)
train_all <- training(split_all)
test_all <- testing(split_all)

reg_rec <- recipe(PPC~
                    CLINIC_LOCATION_RETAIL_PARTNER + CLINIC_LOCATION_STATE + 
                    ALL_OWNERS_IN_5_MILES + ACTIVE_CC_WITHIN_5_MILES + ACTIVE_WC_WITHIN_5_MILES + 
                    NEAREST_COLLEGE + ALL_OWNERS_IN_30_MILES + HEAT_WOOD + MILITARY_BASES_IN_20_MILES + 
                    HAWAIIAN + ACTIVE_WC_WITHIN_15_MILES + ASIAN + AGE_5_TO_9 + 
                    AVERAGE_HH_SIZE + ACTIVE_VETS_IN_5_MILES + HU_VACANT + `_2_RACES` + 
                    ACTIVE_SAME_STORE_LOCATIONS_5_MILES
                  ,data = train_all) %>%
  step_nzv(c(ALL_OWNERS_IN_5_MILES,ACTIVE_CC_WITHIN_5_MILES,ACTIVE_WC_WITHIN_5_MILES,NEAREST_COLLEGE,
             ALL_OWNERS_IN_30_MILES,HEAT_WOOD,MILITARY_BASES_IN_20_MILES,HAWAIIAN,ACTIVE_WC_WITHIN_15_MILES,
             ASIAN,AGE_5_TO_9,AVERAGE_HH_SIZE,ACTIVE_VETS_IN_5_MILES,HU_VACANT,`_2_RACES`,ACTIVE_SAME_STORE_LOCATIONS_5_MILES)) %>%
  step_dummy(c(CLINIC_LOCATION_RETAIL_PARTNER,CLINIC_LOCATION_STATE), -all_outcomes()) %>% 
  step_normalize(c(ALL_OWNERS_IN_5_MILES,ACTIVE_CC_WITHIN_5_MILES,ACTIVE_WC_WITHIN_5_MILES,NEAREST_COLLEGE,
                   ALL_OWNERS_IN_30_MILES,HEAT_WOOD,MILITARY_BASES_IN_20_MILES,HAWAIIAN,ACTIVE_WC_WITHIN_15_MILES,
                   ASIAN,AGE_5_TO_9,AVERAGE_HH_SIZE,ACTIVE_VETS_IN_5_MILES,HU_VACANT,`_2_RACES`,ACTIVE_SAME_STORE_LOCATIONS_5_MILES)) %>%
  prep()


###############################
## BASE XGB REGRESSION MODEL ##
###############################

xgb_spec <- 
  boost_tree() %>% 
  set_mode("regression") %>%
  set_engine("xgboost")

xgb_wflow <-
  workflow() %>%
  add_recipe(tidy_rec) %>% 
  add_model(xgb_spec)

xgb_res <- 
  xgb_wflow %>% 
  fit_resamples(
    resamples = tidy_kfolds,
    metrics = metric_set(rmse, rsq, mae),
    control = control_resamples(save_pred = TRUE)) 

xgb_metrics <- xgb_res %>% 
  collect_metrics(summarize = TRUE) %>%
  mutate(model = "XGBoost")
xgb_metrics

xgb_last_fit <- last_fit(xgb_wflow, split = split_all)
xgb_last_fit %>% collect_metrics()

# NOW TO ESTABLISH THE ABOVE MODEL AND TEST IT ON NEW DATA

xgb_boosted_param <- xgb_res %>%
  select_best("rmse")

xgb_model_final <- finalize_model(xgb_spec,xgb_boosted_param)

xgb_trained_mod <- workflow() %>%
  add_recipe(reg_rec) %>%
  add_model(xgb_model_final) %>%
  fit(data = train_all)

xgb_test_pred <- xgb_trained_mod %>%
  predict(new_data = test_all) %>%
  bind_cols(test_all)

xgb_test_pred <- xgb_test_pred %>%
  yardstick::metrics(PPC, .pred) %>%
  mutate(.estimate = format(round(.estimate, 2), big.mark = ","),model = "XGBoost") %>%
  data.frame()

xgb_test_pred

# xgb_trained_mod %>% extract_fit_parsnip()%>% vip(num_features=100)

#####################################
## HYPERTUNED XGB REGRESSION MODEL ##
#####################################

tuned_xgb_spec <-
  boost_tree(trees = 1000,
             min_n = tune(),
             tree_depth = tune(),
             learn_rate = tune(),
             loss_reduction = tune()) %>%
  set_mode("regression") %>%
  set_engine("xgboost", nthread = 1)

tuned_xg_boost_params <- dials::parameters(
  min_n(),
  tree_depth(),
  learn_rate(),
  loss_reduction())

tuned_xgboost_grid <-
  dials::grid_max_entropy(
    tuned_xg_boost_params,
    size = 50)
system.time({
  tuned_xgb <- tune::tune_grid(tuned_xgb_spec,
                               tidy_rec,
                               resamples = tidy_kfolds,
                               grid = tuned_xgboost_grid,
                               metrics = yardstick::metric_set(rmse, rsq, mae, ccc),
                               control = tune::control_grid(verbose = TRUE))})
show_best(tuned_xgb, "rmse", 10)
tuned_boosted_param <- tuned_xgb %>%
  select_best("rmse")

tuned_xgboost_model <- finalize_model(tuned_xgb_spec, tuned_boosted_param)

tuned_xgb_wflow <-
  workflow() %>%
  add_recipe(reg_rec) %>%
  add_model(tuned_xgboost_model) %>%
  fit(data = train_all)

tuned_xgb_res <-
  tuned_xgb_wflow %>%
  predict(new_data = test_all) %>%
  bind_cols(test_all)

tuned_xgb_test_pred <- tuned_xgb_res %>%
  yardstick::metrics(PPC, .pred) %>%
  mutate(.estimate = format(round(.estimate, 2), big.mark = ","),model = "XGBoost Tuned") %>%
  data.frame()

tuned_xgb_test_pred

tuned_xgb_wflow %>% extract_fit_parsnip()%>% vip(num_features=50)
test <- tuned_xgb_wflow %>% extract_fit_parsnip()%>% vip(num_features=50)
df <- head(test$data,20)
print(head(test$data,20),n=20)
df$Variable

#################################
## BASE LASSO REGRESSION MODEL ##
#################################

lasso_spec <- 
  linear_reg(penalty = 0.1, mixture = 1) %>%
  set_mode("regression") %>%
  set_engine("glmnet")

lasso_wflow <- 
  workflow() %>%
  add_recipe(tidy_rec) %>%
  add_model(lasso_spec) 

lasso_res <- 
  lasso_wflow %>% 
  fit_resamples(
    resamples = tidy_kfolds,
    metrics = metric_set(rmse, rsq, mae),
    control = control_resamples(save_pred = TRUE)) 

lasso_metrics <- lasso_res %>% 
  collect_metrics(summarize = TRUE) %>%
  mutate(model = "Lasso")
lasso_metrics

lasso_last_fit <- last_fit(lasso_wflow, split = split_all)
lasso_last_fit %>% collect_metrics()

# NOW TO ESTABLISH THE ABOVE MODEL AND TEST IT ON NEW DATA

lasso_boosted_param <- lasso_res %>%
  select_best("rmse")

lasso_model_final <- finalize_model(lasso_spec,lasso_boosted_param)

lasso_trained_mod <- workflow() %>%
  add_recipe(reg_rec) %>%
  add_model(lasso_model_final) %>%
  fit(data = train_all)

lasso_test_pred <- lasso_trained_mod %>%
  predict(new_data = test_all) %>%
  bind_cols(test_all)

lasso_test_pred <- lasso_test_pred %>%
  yardstick::metrics(PPC, .pred) %>%
  mutate(.estimate = format(round(.estimate, 2), big.mark = ","),model = "Lasso") %>%
  data.frame()

lasso_test_pred


lasso_trained_mod %>% extract_fit_parsnip()%>% vip(num_features=50)
test <- lasso_trained_mod %>% extract_fit_parsnip()%>% vip(num_features=50)
df <- head(test$data,20)
print(head(test$data,20),n=20)
df$Variable

#######################################
## HYPERTUNED LASSO REGRESSION MODEL ##
#######################################

tuned_lasso_spec <- 
  linear_reg(penalty = tune(), mixture = 1) %>%
  set_mode("regression") %>%
  set_engine("glmnet")

tuned_lasso_wflow <- workflow() %>%
  add_recipe(tidy_rec) %>%
  add_model(tuned_lasso_spec)

tuned_lasso_lambda_grid <- grid_regular(penalty(), levels = 50)

# doParallel::registerDoParallel()

set.seed(123)
tuned_lasso_grid <- tune_grid(
  tuned_lasso_wflow,
  resamples = tidy_kfolds,
  grid = tuned_lasso_lambda_grid)

tuned_lasso_boosted_param <- tuned_lasso_grid %>%
  select_best("rmse")

tuned_lasso_model_final <-  finalize_model(
  tuned_lasso_spec,tuned_lasso_boosted_param)

tuned_lasso_trained_mod <- workflow() %>%
  add_recipe(reg_rec) %>%
  add_model(tuned_lasso_model_final) %>%
  fit(data = train_all)

tuned_lasso_test_pred <- tuned_lasso_trained_mod %>%
  predict(new_data = test_all) %>%
  bind_cols(test_all)

tuned_lasso_test_pred <- tuned_lasso_test_pred %>%
  yardstick::metrics(PPC, .pred) %>%
  mutate(.estimate = format(round(.estimate, 2), big.mark = ","),model = "Lasso Tuned") %>%
  data.frame()

tuned_lasso_test_pred


tuned_lasso_trained_mod %>% extract_fit_parsnip()%>% vip(num_features=50)
test <- tuned_lasso_trained_mod %>% extract_fit_parsnip()%>% vip(num_features=50)
df <- head(test$data,50)
print(head(test$data,50),n=50)
df$Variable


#################################
## BASE RIDGE REGRESSION MODEL ##
#################################

ridge_spec <- 
  linear_reg(penalty = 0.1, mixture = 0) %>%
  set_mode("regression") %>%
  set_engine("glmnet")

ridge_wflow <- 
  workflow() %>%
  add_recipe(tidy_rec) %>%
  add_model(ridge_spec) 

ridge_res <- 
  ridge_wflow %>% 
  fit_resamples(
    resamples = tidy_kfolds,
    metrics = metric_set(rmse, rsq, mae),
    control = control_resamples(save_pred = TRUE)) 

ridge_metrics <- ridge_res %>% 
  collect_metrics(summarize = TRUE) %>%
  mutate(model = "Ridge")
ridge_metrics

ridge_last_fit <- last_fit(ridge_wflow, split = split_all)
ridge_last_fit %>% collect_metrics()

# NOW TO ESTABLISH THE ABOVE MODEL AND TEST IT ON NEW DATA

ridge_boosted_param <- ridge_res %>%
  select_best("rmse")

ridge_model_final <- finalize_model(ridge_spec,ridge_boosted_param)

ridge_trained_mod <- workflow() %>%
  add_recipe(reg_rec) %>%
  add_model(ridge_model_final) %>%
  fit(data = train_all)

ridge_test_pred <- ridge_trained_mod %>%
  predict(new_data = test_all) %>%
  bind_cols(test_all)

ridge_test_pred <- ridge_test_pred %>%
  yardstick::metrics(PPC, .pred) %>%
  mutate(.estimate = format(round(.estimate, 2), big.mark = ","),model = "Ridge") %>%
  data.frame()

ridge_test_pred

#######################################
## HYPERTUNED RIDGE REGRESSION MODEL ##
#######################################

tuned_ridge_spec <- 
  linear_reg(penalty = tune(), mixture = 0) %>%
  set_mode("regression") %>%
  set_engine("glmnet")

tuned_ridge_wflow <- workflow() %>%
  add_recipe(tidy_rec) %>%
  add_model(tuned_ridge_spec)

tuned_ridge_lambda_grid <- grid_regular(penalty(), levels = 50)

# doParallel::registerDoParallel()

set.seed(123)
tuned_ridge_grid <- tune_grid(
  tuned_ridge_wflow,
  resamples = tidy_kfolds,
  grid = tuned_ridge_lambda_grid)

tuned_ridge_boosted_param <- tuned_ridge_grid %>%
  select_best("rmse")

tuned_ridge_model_final <-  finalize_model(
  tuned_ridge_spec,tuned_ridge_boosted_param)

tuned_ridge_trained_mod <- workflow() %>%
  add_recipe(reg_rec) %>%
  add_model(tuned_ridge_model_final) %>%
  fit(data = train_all)

tuned_ridge_test_pred <- tuned_ridge_trained_mod %>%
  predict(new_data = test_all) %>%
  bind_cols(test_all)

tuned_ridge_test_pred <- tuned_ridge_test_pred %>%
  yardstick::metrics(PPC, .pred) %>%
  mutate(.estimate = format(round(.estimate, 2), big.mark = ","),model = "Ridge Tuned") %>%
  data.frame()

tuned_ridge_test_pred


tuned_ridge_trained_mod %>% extract_fit_parsnip()%>% vip(num_features=50)
test <- tuned_ridge_trained_mod %>% extract_fit_parsnip()%>% vip(num_features=50)
df <- head(test$data,50)
print(head(test$data,50),n=50)
df$Variable

###########################
## COMPARE MODEL RESULTS ##
###########################

#TRAINED MODEL COMPARISON

model_training_compared <- bind_rows(
  xgb_metrics,
  lasso_metrics,
  ridge_metrics) 

trained_model_comp <- 
  model_training_compared %>% 
  dplyr::select(model, .metric, mean, std_err) %>% 
  pivot_wider(names_from = .metric, values_from = c(mean, std_err)) 

trained_model_comp %>% 
  arrange(mean_rmse) %>% 
  mutate(model = fct_reorder(model, mean_rmse)) %>%
  ggplot(aes(model, mean_rmse, fill=model)) +
  geom_col() +
  coord_flip() +
  scale_fill_brewer(palette = "Blues") + 
  geom_text(
    size = 3,
    aes(label = round(mean_rmse, 2), y = mean_rmse + 0.08),
    vjust = 1)

trained_model_comp

#TESTED MODEL COMPARISON

model_test_compared <- bind_rows(
  xgb_test_pred,
  tuned_xgb_test_pred,
  lasso_test_pred,
  tuned_lasso_test_pred,
  ridge_test_pred,
  tuned_ridge_test_pred)

model_test_compared
tested_model_comp <- 
  model_test_compared %>% 
  dplyr::select(model, .metric, .estimate) %>% 
  pivot_wider(names_from = .metric, values_from = c(.estimate)) 

tested_model_comp %>% 
  arrange(rsq) %>% 
  mutate(model = fct_reorder(model, rsq)) %>%
  ggplot(aes(model, rsq, fill=model)) +
  geom_col() +
  coord_flip() +
  scale_fill_brewer(palette = "Blues") + 
  geom_text(
    size = 3,
    aes(label = rsq),
    vjust = 1)

# > model_test_compared
# .metric .estimator .estimate         model
# 1     rmse   standard      3.88       XGBoost
# 2      rsq   standard      0.53       XGBoost
# 3      mae   standard      2.95       XGBoost
# 4     rmse   standard      3.80 XGBoost Tuned
# 5      rsq   standard      0.55 XGBoost Tuned
# 6      mae   standard      2.87 XGBoost Tuned
# 7     rmse   standard      4.48         Lasso
# 8      rsq   standard      0.37         Lasso
# 9      mae   standard      3.52         Lasso
# 10    rmse   standard      4.50   Lasso Tuned
# 11     rsq   standard      0.37   Lasso Tuned
# 12     mae   standard      3.51   Lasso Tuned
# 13    rmse   standard      4.48         Ridge
# 14     rsq   standard      0.37         Ridge
# 15     mae   standard      3.52         Ridge
# 16    rmse   standard      4.48   Ridge Tuned
# 17     rsq   standard      0.37   Ridge Tuned
# 18     mae   standard      3.52   Ridge Tuned

tuned_xgb_res_pred <-
  tuned_xgb_wflow %>%
  predict(new_data = test_all) %>%
  bind_cols(test_all)

ggplot(data = tuned_xgb_res_pred,
       mapping = aes(x = .pred, y = PPC)) +
  geom_point(color = '#006EA1') +
  geom_abline(intercept = 0, slope = 1, color = 'red') +
  geom_abline(intercept = 0+sqrt(mean((tuned_xgb_res_pred$PPC-tuned_xgb_res_pred$.pred)^2)), slope = 1, color = 'black') +
  geom_abline(intercept = 0-sqrt(mean((tuned_xgb_res_pred$PPC-tuned_xgb_res_pred$.pred)^2)), slope = 1, color = 'black') +
  labs(x = 'Predictions',
       y = 'Actuals')

sqrt(mean((tuned_xgb_res_pred$PPC-tuned_xgb_res_pred$.pred)^2))
# [1] 3.80

data %>% filter(CLINIC_LOCATION_STATE =='UT' & CLINIC_LOCATION_RETAIL_PARTNER == 'Tractor Supply Company')

temp_test <- read_excel("C:/Users/kyle.chauvin/OneDrive - PetIQ, LLC/Excel/Test Proposal.xlsx")
temp_test <- temp_test %>% mutate(across(where(is.character), as.factor))
temp_test <- temp_test %>% mutate(across(where(is.logical), as.factor))

test123 <-
  tuned_xgb_wflow %>%
  predict(new_data = temp_test) %>%
  bind_cols(temp_test)

print(test123,n=ncol(test123))

temp_test2 <- main_data %>% filter(CLINIC_LOCATION_STATE=='UT')
test456 <-
  tuned_xgb_wflow %>%
  predict(new_data = temp_test2) %>%
  bind_cols(temp_test2)
test456[,c(1,22)]

# PPC~ CLINIC_LOCATION_RETAIL_PARTNER + CLINIC_LOCATION_STATE + 
#   ALL_OWNERS_IN_5_MILES + ACTIVE_CC_WITHIN_5_MILES + ACTIVE_WC_WITHIN_5_MILES + 
#   NEAREST_COLLEGE + ALL_OWNERS_IN_30_MILES + HEAT_WOOD + MILITARY_BASES_IN_20_MILES + 
#   HAWAIIAN + ACTIVE_WC_WITHIN_15_MILES + ASIAN + AGE_5_TO_9 + 
#   AVERAGE_HH_SIZE + ACTIVE_VETS_IN_5_MILES + HU_VACANT + `_2_RACES` + 
#   ACTIVE_SAME_STORE_LOCATIONS_5_MILES

# PPC~
#   BRAND_NAME+ ALL_OWNERS_IN_5_MILES+ CLOSEST_MC_BASE+ ACTIVE_CC_WITHIN_5_MILES+
#   ACTIVE_WC_WITHIN_5_MILES+ CLINIC_LOCATION_CAN_SELL_NAIL_TRIMS+ NEAREST_COLLEGE