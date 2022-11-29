### WORKING SESSION
rm(list = ls())
options(scipen = 100);options(max.print = 10000)
library(readxl);library(dplyr);library(caret);library(ggplot2);library(lattice);library(rpart);
library(Metrics);library(glmnet);library(Matrix);library(caret);library(kableExtra);library(class)
library(gmodels);library(MASS);library(reshape2);library(tidyr);library(ggpubr);library(tibble)
library(sqldf);library(car);library(ggpubr);library(tidyverse);library(stringr);library(data.table)
library(factoextra);library(writexl); library(cluster);library(corrplot);library(tidyverse)
library(dbplyr);library(DBI);library(odbc);library(xgboost);library(gridExtra);library(doParallel)
library(arrow);library(tidymodels);library(doFuture);library(vip);library(keras);library(tensorflow)
set.seed(123)

###############################################################################
######  STEP 2 : TAKE PREVIOUSLY LOADED DATA AND CLEAN IT FOR LATER USE  ######
###############################################################################

data <- read_feather("Data/Medical_Cost_Dataset_Feather")
data <- data.frame(data)

data <- data %>% mutate(
  sex = as.factor(sex),
  smoker = as.factor(smoker),
  region = as.factor(region)
)

# Taking the first look at our data
summary(data)


# quartiles <- quantile(data$charges, probs=c(.25, .75), na.rm = TRUE)
# IQR <- IQR(data$charges,na.rm = TRUE)
# Lower <- quartiles[1] - 1.5*IQR
# Upper <- quartiles[2] + 1.5*IQR
# data <- subset(data, data$charges > Lower & data$charges < Upper)
# 
# boxplot(data$charges)




summary(lm(charges~.,data=data))






tidy_split <- initial_split(data, prop = 7 / 10)
tidy_train <- training(tidy_split)
tidy_test <- testing(tidy_split)
tidy_kfolds <- vfold_cv(tidy_train, 5)

tidy_rec <- recipe(charges ~.,
                   data = tidy_train) %>%
  step_nzv(all_predictors()) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>% 
  step_normalize(all_predictors()) %>%
  prep()


split_all <- initial_split(data, prop = 7 / 10)
train_all <- training(split_all)
test_all <- testing(split_all)

reg_rec <- recipe(charges ~.,
                  data = train_all) %>%
  step_nzv(all_predictors()) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>% 
  step_normalize(all_predictors()) %>%
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
  yardstick::metrics(charges, .pred) %>%
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
  yardstick::metrics(charges, .pred) %>%
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
  yardstick::metrics(charges, .pred) %>%
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
  yardstick::metrics(charges, .pred) %>%
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
  yardstick::metrics(charges, .pred) %>%
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
  yardstick::metrics(charges, .pred) %>%
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



tested_model_comp

# model         rmse     rsq        mae     
# <chr>         <chr>    <chr>      <chr>   
# 1 XGBoost       5,168.81 "    0.80" 2,693.01
# 2 XGBoost Tuned 5,008.48 "    0.82" 2,455.32
# 3 Lasso         6,398.31 "    0.70" 4,261.80
# 4 Lasso Tuned   6,398.31 "    0.70" 4,261.80
# 5 Ridge         6,372.62 "    0.70" 4,302.26
# 6 Ridge Tuned   6,372.62 "    0.70" 4,302.26