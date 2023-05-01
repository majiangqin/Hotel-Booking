#Clear environment and set seed
rm(list = ls())
set.seed(42)

#Load required packages
library(dplyr)
library(glmnet)
library(ggplot2)
library(caret)
library(MASS)

#Read-in datasets (exported from EDA code as .csv files with same names as data frames)
write.csv(City_Data_raw, "C:/Users/Dakota Coomes/Documents/R Working Directory/MGT 6203 Time Series and Logistic Regression Data/City_Data_raw.csv", row.names=FALSE)
write.csv(Resort_Data_raw, "C:/Users/Dakota Coomes/Documents/R Working Directory/MGT 6203 Time Series and Logistic Regression Data/Resort_Data_raw.csv", row.names=FALSE)
write.csv(HotelData_Final, "C:/Users/Dakota Coomes/Documents/R Working Directory/MGT 6203 Time Series and Logistic Regression Data/HotelData_Final.csv", row.names=FALSE)
city <- read.csv("City_Data_raw.csv", header = TRUE)
resort <- read.csv("Resort_Data_raw.csv", header = TRUE)
city_resort <- read.csv("HotelData_Final.csv", header = TRUE)


#Create dummy variables
city$is_canceled <- as.factor(city$is_canceled)
city$market_segment <- as.factor(city$market_segment)
city$is_repeated_guest <- as.factor(city$is_repeated_guest)
city$deposit_type <- as.factor(city$deposit_type)
city$Season <- as.factor(city$Season)
levels(city$market_segment)
levels(city$deposit_type)
levels(city$Season)

resort$is_canceled <- as.factor(resort$is_canceled)
resort$market_segment <- as.factor(resort$market_segment)
resort$is_repeated_guest <- as.factor(resort$is_repeated_guest)
resort$deposit_type <- as.factor(resort$deposit_type)
resort$Season <- as.factor(resort$Season)


city_resort$is_canceled <- as.factor(city_resort$is_canceled)
city_resort$market_segment <- as.factor(city_resort$market_segment)
city_resort$is_repeated_guest <- as.factor(city_resort$is_repeated_guest)
city_resort$deposit_type <- as.factor(city_resort$deposit_type)
city_resort$Season <- as.factor(city_resort$Season)

city <- city %>%
  mutate(Summer = ifelse(Season == "Summer",1,0)) %>%
  mutate(Spring = ifelse(Season == "Spring",1,0)) %>%
  mutate(Fall = ifelse(Season == "Fall",1,0)) %>%
  mutate(Winter = ifelse(Season == "Winter",1,0)) %>%
  mutate(Groups = ifelse(market_segment == "Groups",1,0)) %>%
  mutate(Offline_TA = ifelse(market_segment == "Offline TA/TO",1,0)) %>%
  mutate(Online_TA = ifelse(market_segment == "Online TA",1,0)) %>%
  mutate(Direct = ifelse(market_segment == "Direct",1,0)) %>%
  mutate(Corporate = ifelse(market_segment == "Corporate",1,0)) %>%
  mutate(Complementary = ifelse(market_segment == "Complementary",1,0)) %>%
  mutate(Aviation = ifelse(market_segment == "Aviation",1,0)) %>%
  mutate(Repeated = ifelse(is_repeated_guest == "1",1,0)) %>%
  mutate(Not_Repeated = ifelse(is_repeated_guest == "0",1,0)) %>%
  mutate(No_Deposit = ifelse(deposit_type == "No Deposit",1,0)) %>%
  mutate(Nonrefundable = ifelse(deposit_type == "Non Refund",1,0)) %>%
  mutate(Refundable = ifelse(deposit_type == "Refundable",1,0))

resort <- resort %>%
  mutate(Summer = ifelse(Season == "Summer",1,0)) %>%
  mutate(Spring = ifelse(Season == "Spring",1,0)) %>%
  mutate(Fall = ifelse(Season == "Fall",1,0)) %>%
  mutate(Winter = ifelse(Season == "Winter",1,0)) %>%
  mutate(Groups = ifelse(market_segment == "Groups",1,0)) %>%
  mutate(Offline_TA = ifelse(market_segment == "Offline TA/TO",1,0)) %>%
  mutate(Online_TA = ifelse(market_segment == "Online TA",1,0)) %>%
  mutate(Direct = ifelse(market_segment == "Direct",1,0)) %>%
  mutate(Corporate = ifelse(market_segment == "Corporate",1,0)) %>%
  mutate(Complementary = ifelse(market_segment == "Complementary",1,0)) %>%
  mutate(Aviation = ifelse(market_segment == "Aviation",1,0)) %>%
  mutate(Repeated = ifelse(is_repeated_guest == "1",1,0)) %>%
  mutate(Not_Repeated = ifelse(is_repeated_guest == "0",1,0)) %>%
  mutate(No_Deposit = ifelse(deposit_type == "No Deposit",1,0)) %>%
  mutate(Nonrefundable = ifelse(deposit_type == "Non Refund",1,0)) %>%
  mutate(Refundable = ifelse(deposit_type == "Refundable",1,0))

city_resort <- city_resort %>%
  mutate(Summer = ifelse(Season == "Summer",1,0)) %>%
  mutate(Spring = ifelse(Season == "Spring",1,0)) %>%
  mutate(Fall = ifelse(Season == "Fall",1,0)) %>%
  mutate(Winter = ifelse(Season == "Winter",1,0)) %>%
  mutate(Groups = ifelse(market_segment == "Groups",1,0)) %>%
  mutate(Offline_TA = ifelse(market_segment == "Offline TA/TO",1,0)) %>%
  mutate(Online_TA = ifelse(market_segment == "Online TA",1,0)) %>%
  mutate(Direct = ifelse(market_segment == "Direct",1,0)) %>%
  mutate(Corporate = ifelse(market_segment == "Corporate",1,0)) %>%
  mutate(Complementary = ifelse(market_segment == "Complementary",1,0)) %>%
  mutate(Aviation = ifelse(market_segment == "Aviation",1,0)) %>%
  mutate(Repeated = ifelse(is_repeated_guest == "1",1,0)) %>%
  mutate(Not_Repeated = ifelse(is_repeated_guest == "0",1,0)) %>%
  mutate(No_Deposit = ifelse(deposit_type == "No Deposit",1,0)) %>%
  mutate(Nonrefundable = ifelse(deposit_type == "Non Refund",1,0)) %>%
  mutate(Refundable = ifelse(deposit_type == "Refundable",1,0))

#Split into training and test datasets
city_train <- sample(1:nrow(city), as.integer(0.75*nrow(city)))
city_train_df <- city[city_train,]
city_test_df <- city[-city_train,]
resort_train <- sample(1:nrow(resort), as.integer(0.75*nrow(resort)))
resort_train_df <- resort[resort_train,]
resort_test_df <- resort[-resort_train,]
city_resort_train <- sample(1:nrow(city_resort), as.integer(0.75*nrow(city_resort)))
city_resort_train_df <- city_resort[city_resort_train,]
city_resort_test_df <- city_resort[-city_resort_train,]

#Full logistic regression (defaults are Spring, Direct, Not_Repeated, No_Deposit)
city_full <- glm(is_canceled ~ lead_time + stays_in_weekend_nights + stays_in_week_nights + adults + children + adr + Summer + Fall + Winter + Groups + Offline_TA + Online_TA + Corporate + Complementary + Aviation + Repeated + Nonrefundable + Refundable, data = city_train_df, family = "binomial")
summary(city_full)

resort_full <- glm(is_canceled ~ lead_time + stays_in_weekend_nights + stays_in_week_nights + adults + children + adr + Summer + Fall + Winter + Groups + Offline_TA + Online_TA + Corporate + Complementary + Repeated + Nonrefundable + Refundable, data = resort_train_df, family = "binomial")
summary(resort_full)

city_resort_full <- glm(is_canceled ~ lead_time + stays_in_weekend_nights + stays_in_week_nights + adults + children + adr + Summer + Fall + Winter + Groups + Offline_TA + Online_TA + Corporate + Complementary + Aviation + Repeated + Nonrefundable + Refundable, data = city_resort_train_df, family = "binomial")
summary(city_resort_full)

#Stepwise logistic regression
city_step <- glm(is_canceled ~ lead_time + stays_in_weekend_nights + stays_in_week_nights + adults + children + adr + Summer + Fall + Winter + Groups + Offline_TA + Online_TA + Corporate + Complementary + Aviation + Repeated + Nonrefundable + Refundable, data = city_train_df, family = "binomial") %>%
  stepAIC(trace = FALSE)
summary(city_step)

resort_step <- glm(is_canceled ~ lead_time + stays_in_weekend_nights + stays_in_week_nights + adults + children + adr + Summer + Fall + Winter + Groups + Offline_TA + Online_TA + Corporate + Complementary + Repeated + Nonrefundable + Refundable, data = resort_train_df, family = "binomial") %>%
  stepAIC(trace = FALSE)
summary(resort_step)

city_resort_step <- glm(is_canceled ~ lead_time + stays_in_weekend_nights + stays_in_week_nights + adults + children + adr + Summer + Fall + Winter + Groups + Offline_TA + Online_TA + Corporate + Complementary + Aviation + Repeated + Nonrefundable + Refundable, data = city_resort_train_df, family = "binomial") %>%
  stepAIC(trace = FALSE)
summary(city_resort_step)

#Predictions
city_full_pred <- city_full %>% predict(city_test_df, type = "response")
city_full_pred_classes <- ifelse(city_full_pred > 0.5, 1, 0)
city_full_observed_classes <- city_test_df$is_canceled
mean(city_full_pred_classes == city_full_observed_classes)

resort_full_pred <- resort_full %>% predict(resort_test_df, type = "response")
resort_full_pred_classes <- ifelse(resort_full_pred > 0.5, 1, 0)
resort_full_observed_classes <- resort_test_df$is_canceled
mean(resort_full_pred_classes == resort_full_observed_classes)

city_resort_full_pred <- city_resort_full %>% predict(city_resort_test_df, type = "response")
city_resort_full_pred_classes <- ifelse(city_resort_full_pred > 0.5, 1, 0)
city_resort_full_observed_classes <- city_resort_test_df$is_canceled
mean(city_resort_full_pred_classes == city_resort_full_observed_classes)

city_step_pred <- city_step %>% predict(city_test_df, type = "response")
city_step_pred_classes <- ifelse(city_step_pred > 0.5, 1, 0)
city_step_observed_classes <- city_test_df$is_canceled
mean(city_step_pred_classes == city_step_observed_classes)

resort_step_pred <- resort_step %>% predict(resort_test_df, type = "response")
resort_step_pred_classes <- ifelse(resort_step_pred > 0.5, 1, 0)
resort_step_observed_classes <- resort_test_df$is_canceled
mean(resort_step_pred_classes == resort_step_observed_classes)

city_resort_step_pred <- city_resort_step %>% predict(city_resort_test_df, type = "response")
city_resort_step_pred_classes <- ifelse(city_resort_step_pred > 0.5, 1, 0)
city_resort_step_observed_classes <- city_resort_test_df$is_canceled
mean(city_resort_step_pred_classes == city_resort_step_observed_classes)

#ROC Curves
library(pROC)

city_full_roc <- roc(city_test_df$is_canceled ~ city_full_pred, plot = TRUE, print.auc = TRUE, main = "City Hotel - All Predictors")
resort_full_roc <- roc(resort_test_df$is_canceled ~ resort_full_pred, plot = TRUE, print.auc = TRUE, main = "Resort Hotel - All Predictors")
city_resort_full_roc <- roc(city_resort_test_df$is_canceled ~ city_resort_full_pred, plot = TRUE, print.auc = TRUE, main = "Generalized Hotels - All Predictors")
city_step_roc <- roc(city_test_df$is_canceled ~ city_step_pred, plot = TRUE, print.auc = TRUE, main = "City Hotel - Stepwise")
resort_step_roc <- roc(resort_test_df$is_canceled ~ resort_step_pred, plot = TRUE, print.auc = TRUE, main = "Resort Hotel - Stepwise")
city_resort_step_roc <- roc(city_resort_test_df$is_canceled ~ city_resort_step_pred, plot = TRUE, print.auc = TRUE, main = "Generalized Hotels - Stepwise")
