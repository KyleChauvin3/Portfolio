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
library(timetk);library(moments);library(leaflet);library(leaflet.extras)
################################################################################################################
set.seed(123)

db_connection2 <- DBI::dbConnect(odbc::odbc(),
                                Driver="SNowflakeDSIIDriver",
                                Server="va71008.east-us-2.azure.snowflakecomputing.com",
                                Database="PIQ_ANALYTICS_WORKSPACE",
                                SCHEMA="DEV",
                                UID="Kyle.Chauvin@petiq.com",
                                role="DATAANALYST",
                               authenticator="externalbrowser",
                                WAREHOUSE="PIQWH_ANALYTICS")

CC_main_data_source3 <- DBI::dbGetQuery(db_connection2,
                                    "SELECT * FROM KC_COMMUNITY_CLINIC_DATA")
WC_main_data_source3 <- DBI::dbGetQuery(db_connection2,
                                     "SELECT * FROM KC_WELLNESS_CENTER_DATA")
main_data_source2 <- DBI::dbGetQuery(db_connection2,
                                     "SELECT * FROM KC_OWNERS_DATA")

# ################################################################################################################

# ALL OWNERS
all_owners_main_data <- main_data_source2
colnames(all_owners_main_data)
summary(as.factor(all_owners_main_data$STATE))
all_owners_main_data <- all_owners_main_data %>% filter(is.na(LONGITUDE)==FALSE &
                                                          is.na(LATITUDE)==FALSE &
                                                          STATE %in% c('UT','MO')) #=='UT')
# CC_OWNERS <- all_owners_main_data %>% filter(VISIT_TYPES %in%c("CC Visits","BOTH"))
# WC_OWNERS <- all_owners_main_data %>% filter(VISIT_TYPES %in%c("WC Visits","BOTH"))
# ALL_OWNERS <- all_owners_main_data %>% filter(VISIT_TYPES=="BOTH")


# COMMUNITY CLINICS
CC_main_data3 <- CC_main_data_source3
colnames(CC_main_data3)
summary(as.factor(CC_main_data3$OFFICE_NAME))
CC_main_data3 <- CC_main_data3 %>% filter(is.na(CLINIC_LOCATION_LONGITUDE)==FALSE & 
                                            is.na(CLINIC_LOCATION_LATITUDE)==FALSE &
                                            OFFICE_NAME %in% c('Salt Lake City, UT','St. Louis, MO') & 
                                            LAST_REVENUE_DATE >= '2023-01-01')
CC_main_data3$CITY_STATE <-gsub(" ,",",",paste(CC_main_data3$CLINIC_LOCATION_CITY, ",", CC_main_data3$CLINIC_LOCATION_STATE)) 
CC_main_data3 <- CC_main_data3 %>% filter(CLINIC_LOCATION_ID !=5868)

CC_main_data3$Maturity <- round(difftime(as.Date(CC_main_data3$LAST_REVENUE_DATE),
                                         as.Date(CC_main_data3$FIRST_REVENUE_DATE),
                                         units="weeks")/365,2)


CCgetColor <- function(CC_main_data3) {
  sapply(CC_main_data3$PPC, function(PPC) {
    if(PPC <= 7) {
      "red"
    } else if(PPC <= 14) {
      "orange"
    } else {
      "green"
    } })
}


# WELLNESS CENTERS
WC_main_data3 <- WC_main_data_source3
colnames(WC_main_data3)
summary(as.factor(WC_main_data3$OFFICE_NAME))
WC_main_data3 <- WC_main_data3 %>% filter(is.na(CLINIC_LOCATION_LONGITUDE)==FALSE & 
                                            is.na(CLINIC_LOCATION_LATITUDE)==FALSE &
                                            OFFICE_NAME %in% c('Centerville, UT','Lewiston, ME','Bangor',
                                                               'Tempe, AZ',
                                                               'El Mirage, AZ',
                                                               'Mesa, AZ',
                                                               'Chandler, AZ',
                                                               'Tucson, AZ',
                                                               'Queen Creek, AZ',
                                                               'Glendale, AZ') &
                                            LAST_REVENUE_DATE >= '2023-01-01')
WC_main_data3$CITY_STATE <-gsub(" ,",",",paste(WC_main_data3$CLINIC_LOCATION_CITY, ",", WC_main_data3$CLINIC_LOCATION_STATE)) 

WC_main_data3$Maturity <- round(difftime(as.Date(WC_main_data3$LAST_REVENUE_DATE),
                                         as.Date(WC_main_data3$FIRST_REVENUE_DATE),
                                         units="weeks")/365,2)

WCgetColor <- function(WC_main_data3) {
  sapply(WC_main_data3$PPC, function(PPC) {
    if(PPC <= 7) {
      "red"
    } else if(PPC <= 14) {
      "orange"
    } else {
      "green"
    } })
}



pal <- colorNumeric(palette = c("red", "green"), domain = c(80,120))

# master_plot <- leaflet()
master_plot <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron)  %>% # "Esri.WorldImagery" # providers$CartoDB.PositronNoLabels
  setView(lng = -111.0937, lat = 39.3210, zoom = 6) %>%
  
  # ALL OWNERS
  addCircles(data = all_owners_main_data,
             lng = ~LONGITUDE,
             lat = ~LATITUDE,
             radius = .5,
             color = ~pal(case_when(all_owners_main_data$DPP_L3_YEARS<80~80,
                                    all_owners_main_data$DPP_L3_YEARS>120~120,
                                    TRUE~all_owners_main_data$DPP_L3_YEARS)),
             group = 'All Owners') %>%
  addHeatmap(data = all_owners_main_data,
             lng = ~LONGITUDE,
             lat = ~LATITUDE,
             blur = 30, max = 5, radius = 20,
             group = 'All Owners - Spatial') %>%
  
  # COMMUNITY CLINICS
  addCircles(data = CC_main_data3,
             lng = ~CLINIC_LOCATION_LONGITUDE,
             lat = ~CLINIC_LOCATION_LATITUDE,
             radius = CC_main_data3$MEDIAN_OWNER_DIST_PAST_YEAR * 1609.344, 
             col = ~CCgetColor(CC_main_data3),
             label = ~paste(CC_main_data3$MEDIAN_OWNER_DIST_PAST_YEAR," Mile Radius"),
             group = 'Community Clinic Locations') %>%
  addCircleMarkers(data = CC_main_data3,
                   lng = ~CLINIC_LOCATION_LONGITUDE,
                   lat = ~CLINIC_LOCATION_LATITUDE,
                   radius = ~1,
                   color = ~CCgetColor(CC_main_data3),
                   label = ~gsub(":",": ",paste(CLINIC_LOCATION_ID,":",CITY_STATE,"-",round(as.numeric(PPC),2))),
                   popup = ~paste("<br>Address:",CLINIC_LOCATION_ADDRESS,
                                  "<br>City State:",CITY_STATE,
                                  "<br>Retail Partner:", CLINIC_LOCATION_RETAIL_PARTNER,
                                  "<br>Office Type:", "Community Clinic",
                                  # "<br>Maturity:", Maturity," Years",
                                  "<br>PPC:", round(PPC,2),
                                  "<br>DPC:", "$",round(DPC,2),
                                  "<br>DPP", "$",round(DPP,2)),
                   group = 'Community Clinic Locations') %>%
  
  # WELLNESS CENTERS
  addCircles(data = WC_main_data3,
             lng = ~CLINIC_LOCATION_LONGITUDE,
             lat = ~CLINIC_LOCATION_LATITUDE,
             radius = ~MEDIAN_OWNER_DIST_PAST_YEAR * 1609.344,
             color = ~WCgetColor(WC_main_data3),
             label = ~paste(MEDIAN_OWNER_DIST_PAST_YEAR," Mile Radius"),
             group = 'Wellness Center Locations') %>%
  addCircleMarkers(data = WC_main_data3,
                   lng = ~CLINIC_LOCATION_LONGITUDE,
                   lat = ~CLINIC_LOCATION_LATITUDE,
                   radius = 1,
                   color = ~WCgetColor(WC_main_data3),
                   label = ~gsub(":",": ",paste(CLINIC_LOCATION_ID,":",CITY_STATE,"-",round(as.numeric(PPC),2))),
                   popup = ~paste("<br>Address:",CLINIC_LOCATION_ADDRESS,
                                  "<br>City State:",CITY_STATE,
                                  "<br>Retail Partner:", CLINIC_LOCATION_RETAIL_PARTNER,
                                  "<br>Office Type:", "Wellness Center",
                                  # "<br>Maturity:", Maturity," Years",
                                  "<br>PPC:", round(PPC,2),
                                  "<br>DPC:", "$",round(DPC,2),
                                  "<br>DPP", "$",round(DPP,2)),
                   group = 'Wellness Center Locations') %>%

  
  
  # FILTERABLE LEGEND
  addLayersControl(overlayGroups = c('Community Clinic Locations'
                                     ,'Wellness Center Locations'
                                     ,'All Owners','All Owners - Spatial'
  ),
  options = layersControlOptions(collapsed = FALSE),
  position = 'bottomleft')

master_plot
