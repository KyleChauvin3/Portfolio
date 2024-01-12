rm(list = ls())
QUERY_START_TIME <- Sys.time()
options(scipen = 100);options(max.print = 10000)
library(readxl);library(dplyr);library(caret);library(ggplot2);library(lattice);library(rpart);library(lubridate);
library(Metrics);library(glmnet);library(Matrix);library(caret);library(kableExtra);library(class);library(openxlsx);
library(gmodels);library(MASS);library(reshape2);library(tidyr);library(ggpubr);library(tibble);library(stringr);
library(sqldf);library(car);library(ggpubr);library(tidyverse);library(stringr);library(data.table);library(readxl);
library(factoextra);library(writexl); library(cluster);library(corrplot);library(tidyverse);library(mapsapi);
library(dbplyr);library(DBI);library(odbc);library(xgboost);library(gridExtra);library(tidyverse);library(keras);
library(arrow);library(tidymodels);library(doFuture);library(vip);library(gdata);library(xml2);library(tensorflow);
library(tseries);library(zoo);library(forecast);library(tidyverse);library(tidymodels);library(modeltime);library(timetk);
################################################################################################################
set.seed(123)


data <- read_excel("C:/Users/kyle.chauvin/OneDrive - PetIQ, LLC/Excel/Seasonality.xlsx",sheet = 'Services')


filtered_data <- data[,c(1,3)]
filtered_data$total_pets <- na.approx(filtered_data$total_pets)
filtered_data$Year_Month <- as.Date(as.yearmon(filtered_data$Year_Month))
filtered_data <- filtered_data %>% filter(Year_Month >='2018-01-01') 

filtered_data %>%
  plot_time_series(Year_Month, total_pets, .interactive = FALSE)

splits <- filtered_data %>%
  time_series_split(assess = "15 months", cumulative = TRUE)

splits %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(Year_Month, total_pets, .interactive = FALSE)

#ARIMA
model_fit_arima <- arima_reg() %>%
  set_engine("auto_arima") %>%
  fit(total_pets ~ Year_Month, training(splits))
model_fit_arima

#PROPHET
model_fit_prophet <- prophet_reg() %>%
  set_engine("prophet", yearly.seasonality = TRUE) %>%
  fit(total_pets ~ Year_Month, training(splits))
model_fit_prophet




recipe_spec <- recipe(total_pets ~ Year_Month, training(splits)) %>%
  step_timeseries_signature(Year_Month) %>%
  step_rm(contains("am.pm"), contains("hour"), contains("minute"),
          contains("second"), contains("xts")) %>%
  step_fourier(Year_Month, period = 12, K=5) %>%
  step_dummy(all_nominal())
recipe_spec %>% prep() %>% juice()


model_spec_glmnet <- linear_reg(penalty = 0.01, mixture = 0.5) %>%
  set_engine("glmnet")
workflow_fit_glmnet <- workflow() %>%
  add_model(model_spec_glmnet) %>%
  add_recipe(recipe_spec %>% step_rm(Year_Month)) %>%
  fit(training(splits))


model_spec_rf <- rand_forest(trees = 500, min_n = 50) %>%
  set_engine("randomForest") %>% 
  set_mode("regression")
workflow_fit_rf <- workflow() %>%
  add_model(model_spec_rf) %>%
  add_recipe(recipe_spec %>% step_rm(Year_Month)) %>%
  fit(training(splits))


model_spec_prophet_boost <- prophet_boost() %>%
  set_engine("prophet_xgboost", yearly.seasonality = TRUE) 
workflow_fit_prophet_boost <- workflow() %>%
  add_model(model_spec_prophet_boost) %>%
  add_recipe(recipe_spec) %>%
  fit(training(splits))
workflow_fit_prophet_boost


model_table <- modeltime_table(
  model_fit_arima, 
  model_fit_prophet,
  workflow_fit_glmnet,
  workflow_fit_rf,
  workflow_fit_prophet_boost) 
model_table


calibration_table <- model_table %>%
  modeltime_calibrate(testing(splits))
calibration_table


calibration_table %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(.interactive = FALSE)
# 
calibration_table %>%
  filter(.model_id ==5) %>%
  modeltime_forecast(actual_data = filtered_data) %>%
  plot_modeltime_forecast(.interactive = FALSE)
# 
# 
# 
# 
calibration_table %>%
  filter(.model_id ==5) %>%
  # filter(!.model_id %in% c(1,3,4,5)) %>%
  modeltime_refit(filtered_data) %>%
  modeltime_forecast(h = "12 months", 
                     conf_interval = 0.95,
                     actual_data = filtered_data) %>%
  plot_modeltime_forecast(.interactive = TRUE)


print(
  as.data.frame(
    calibration_table %>%
      filter(.model_id ==5) %>%
      # filter(!.model_id %in% c(1,3,4,5)) %>%
      modeltime_refit(filtered_data) %>%
      modeltime_forecast(h = "12 months", 
                         conf_interval = 0.95,
                         actual_data = filtered_data)
  )
,n=nrow(74))
