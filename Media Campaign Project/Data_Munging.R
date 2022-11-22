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

data <- read_feather("Data/Media_Campaign_Dataset_Feather")
data <- data.frame(data)

# Taking the first look at our data
summary(data)

# Removing duplicate column names
data <- data[,!(names(data) %in% c("avg_cars_at.home.approx....19", "SRP"))]

# Renaming columns to eliminate confusion
data <- data %>% rename(
  store_sales_in_millions =`store_sales.in.millions.`,
  store_cost_in_millions = `store_cost.in.millions.`,
  unit_sales_in_millions = `unit_sales.in.millions.`,
  avg_cars_at_home = `avg_cars_at.home.approx....16`,
  avg_yearly_income_bucket = `avg..yearly_income`,
  is_recyclable_package = recyclable_package,
  is_low_fat = low_fat,
  has_coffee_bar = coffee_bar,
  has_video_store = video_store,
  has_salad_bar = salad_bar,
  has_prepared_food = prepared_food,
  has_florist = florist,
  customer_acquisition_cost = cost
)

# Transforming variables to the correct datatype
data <- data %>% mutate(
  food_category = as.factor(food_category),
  food_department = as.factor(food_department),
  food_family = as.factor(food_family),
  promotion_name = as.factor(promotion_name),
  sales_country = as.factor(sales_country),
  marital_status = as.factor(ifelse(marital_status== "M","Married","Single")),
  gender = as.factor(ifelse(gender == "M","Male","Female")),
  education = as.factor(education),
  member_card = as.factor(member_card),
  occupation = as.factor(occupation),
  houseowner =as.factor(ifelse(houseowner == "Y","Yes","No")),
  is_recyclable_package = as.factor(ifelse(is_recyclable_package == "1","Yes","No")),
  is_low_fat = as.factor(ifelse(is_low_fat == "1","Yes","No")),
  avg_yearly_income_bucket = as.factor(avg_yearly_income_bucket),
  brand_name = as.factor(brand_name),
  store_type = as.factor(store_type),
  store_city = as.factor(store_city),
  store_state = as.factor(store_state),
  has_coffee_bar = as.factor(ifelse(has_coffee_bar == "1","Yes","No")),
  has_video_store = as.factor(ifelse(has_video_store == "1","Yes","No")),
  has_salad_bar = as.factor(ifelse(has_salad_bar == "1","Yes","No")),
  has_prepared_food = as.factor(ifelse(has_prepared_food == "1","Yes","No")),
  has_florist = as.factor(ifelse(has_florist == "1","Yes","No")),
  media_type = as.factor(media_type),
)

str(data)
summary(data)

write_feather(data, "Data/Media_Campaign_Dataset_Feather_Cleaned")
