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

##################################################################
######  STEP 1 : LOAD DATASETS AND SAVE THEM TO REPOSITORY  ######
##################################################################

# https://www.kaggle.com/datasets/gauravduttakiit/media-campaign-cost-prediction
main_data <- read_excel("C:/Users/kyle.chauvin/OneDrive - PetIQ, LLC/Personal/Media_Campaign_Data.xlsx")

write_feather(main_data, "Data/Media_Campaign_Dataset_Feather")


# https://www.kaggle.com/datasets/mirichoi0218/insurance
main_data2 <- read_excel("C:/Users/kyle.chauvin/OneDrive - PetIQ, LLC/Personal/Medical_Cost_Data.xlsx")

write_feather(main_data2, "Data/Medical_Cost_Dataset_Feather")