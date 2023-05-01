#svm model
rm(list = ls())
set.seed(123)
#getwd()
#setwd("/Users/jiangqinma/Documents/R/RWork")
#call the libraries
library(tidyverse)
library(readr)
library(hash)
library(kernlab)
library(caret)
library(class)
library(kknn)
#load data
data1<-read.csv(file='Hotel Reservations.csv')
head(data1)
summary(data1)
str(data1)

#check NA or null values in dataset
sum(is.na(data1))
colSums(is.na(data1))
sum(is.null(data1))

data1$type_of_meal_plan  <- factor(data1$type_of_meal_plan )
data1$room_type_reserved <- factor(data1$room_type_reserved)
data1$arrival_date <- factor(data1$arrival_date)
data1$arrival_month <- factor(data1$arrival_month)
data1$market_segment_type <- factor(data1$market_segment_type)
data1$booking_status <- factor(data1$booking_status)
str(data1)
head(data1)


# Split the data into training and testing sets
train_idx <- sample(nrow(data1), 0.7*nrow(data1))
train_data <- data1[train_idx, ]
test_data <- data1[-train_idx, ]


# Define the tuning parameters
tune_grid <- expand.grid(sigma = c(0.1, 0.01, 0.005),
                         C = c(10, 100, 1))
# initialize variables to store the minimum misclassification rate and corresponding C and sigma values
min_error <- Inf
min_C <- NA
min_sigma <- NA


misclass <- function(predicted, actual) {
  tbl <- table(predicted, actual)
  misclass_rate <- 1 - sum(diag(tbl)) / sum(tbl)
  cat("Table of Misclassification:\n")
  print(tbl)
  cat("\nMisclassification Rate =", misclass_rate, "\n")
  return(misclass_rate)
}

# start the timer
start_time <- Sys.time()

# loop over each combination of C and sigma in tune_grid
for (i in 1:nrow(tune_grid)) {
  sigma <- tune_grid$sigma[i]
  C <- tune_grid$C[i]
  ksvm.tuned = ksvm(booking_status~.,data=as.data.frame(train_data[,2:19]),kpar=list(sigma=sigma),C=C,cross=5,type = "C-svc",scaled = T,kernel="rbfdot")
  yhat = predict(ksvm.tuned,newdata=train_data)
  
  # compute the misclassification rate
  error <- misclass(yhat,train_data$booking_status)
  
  # update the minimum misclassification rate and corresponding C and sigma values if necessary
  if (error < min_error) {
    min_error <- error
    min_C <- C
    min_sigma <- sigma
  }
}

# stop the timer
end_time <- Sys.time()

# calculate the elapsed time
elapsed_time <- end_time - start_time

# print the elapsed time
cat("Elapsed time:", elapsed_time, "\n")

# print the minimum misclassification rate and corresponding C and sigma values
cat("Minimum misclassification rate:", min_error, "\n")
cat("Corresponding C value:", min_C, "\n")
cat("Corresponding sigma value:", min_sigma, "\n")

# Elapsed time: 38.5181 minutes
# Minimum misclassification rate: 0.02705577 
#Corresponding C value: 100
# Corresponding sigma value: 0.1 

min_C<-100
min_sigma<-0.1
# start the timer
start_time <- Sys.time()

#create SVM model with optimal C and sigma values
ksvm.tuned = ksvm(booking_status~.,data=as.data.frame(train_data[,2:19]),kpar=list(sigma=min_sigma),C=min_C,type = "C-svc",scaled = T,kernel="rbfdot")

# stop the timer
end_time <- Sys.time()

# calculate the elapsed time
elapsed_time <- end_time - start_time

# print the elapsed time
cat("Elapsed time:", elapsed_time, "\n")
#Elapsed time: 4.00508 minutes

#see the SVM model characteristics
ksvm.tuned

# Support Vector Machine object of class "ksvm" 
# 
# SV type: C-svc  (classification) 
# parameter : cost C = 100 
# 
# Gaussian Radial Basis kernel function. 
# Hyperparameter : sigma =  0.1 
# 
# Number of Support Vectors : 8283 
# 
# Objective Function Value : -236470.5 
# Training error : 0.0261114

# compute the misclassification rate with test dataset
yhat = predict(ksvm.tuned,newdata=test_data)
misclassification_rate <- misclass(yhat,test_data$booking_status)

# Table of Misclassification:
#   actual
# predicted      Canceled Not_Canceled
# Canceled         2768          693
# Not_Canceled      825         6597

# Misclassification Rate = 0.1394836 
# accuracy= 1 - Misclassification Rate= 86.1%



##############################################################################################

#knn model
# Setting the random number generator seed so that our results are reproducible
set.seed(1)

# set maximum value of k (number of neighbors) to test
kmax <- 30

# create array of misclassification rates
misclass_cv <- rep(0, kmax)

# start the timer
start_time <- Sys.time()

# calculate misclassification rates
for (k in 1:kmax) {
  # run cross-validation for each value of k (number of neighbors)
  model <- cv.kknn(booking_status~., data=train_data[,2:19], kcv = 10, k = k, scale = TRUE)
  predicted <- as.integer(model[[1]][, 2] + 0.5) # round off to 0 or 1
  misclass_cv[k] <- misclass(predicted, train_data$booking_status)
}

# stop the timer
end_time <- Sys.time()

# calculate the elapsed time
elapsed_time <- end_time - start_time

# print the elapsed time
cat("Elapsed time:", elapsed_time, "\n")
#Elapsed time: 11.61355 minutes

# show misclassification rates
misclass_cv

data.frame(k = 1:length(misclass_cv), misclassification = misclass_cv) %>%
  ggplot(aes(x = k, y = misclassification)) +
  geom_line() +
  xlab("Number of neighbors (k)") +
  ylab("misclassification rates")

min(misclass_cv)

which.min(misclass_cv)

# > which.min(misclass_cv)
# [1] 18
# > min(misclass_cv)
# [1] 0.1433522
#so optimal k=18

# start the timer
start_time <- Sys.time()

#create KNN mode with optimal k
kknn.tuned <- kknn(booking_status~., train=train_data[,2:19], test=test_data[,2:19], k = 18, scale = TRUE)
# stop the timer
end_time <- Sys.time()

# calculate the elapsed time
elapsed_time <- end_time - start_time

# print the elapsed time
cat("Elapsed time:", elapsed_time, "\n")
#Elapsed time: 12.39392 seconds

kknn.tuned
summary(kknn.tuned)
fitted(kknn.tuned)
predicted <- fitted(kknn.tuned) # round off to 0 or 1
misclass_cv <- misclass(predicted, test_data$booking_status)
# Table of Misclassification:
#   actual
# predicted      Canceled Not_Canceled
# Canceled         2601          598
# Not_Canceled      992         6692
# 
# Misclassification Rate = 0.1460994 
# accuracy= 1 - Misclassification Rate= 85.4%

###################################################################################
#logistic regression


# Load required packages
library(boot)
library(correlationfunnel)
library(DataExplorer)
library(WVPlots)
library(corrplot)
library(caret)


# I decide to  do feature selection, which is a common technique used
# to reduce the complexity of the model and improve its performance. 
# By doing so, we can potentially reduce the training time of the model 
# and improve its interpretability.


## plot_correlation_funnel() function creates a funnel plot that shows the 
# strength and direction of the correlation between the response 
# variable (booking_status) and each predictor variable in the dataset.
# The y-axis shows the sorted in descending order of the correlation coefficients.
# By looking at this plot, you can identify which predictor variables are 
# strongly correlated (either positively or negatively) with the response 
# variable, and which ones are not significantly correlated. 

data1 %>%
  mutate_if(is.numeric,as.numeric)%>%
  binarize() %>% #bins the data immediately 
  correlate(booking_status__Canceled) %>%  #correlate just creates correlations plots 
  plot_correlation_funnel(interactive = TRUE, alpha = 0.7)


#observe the above correlation funnel plot, we will select the top 11 correlated 
#variables and move forward building a logistic regression model with just them using training dataset.
# I try to decrease or increase the number of selected features, but the accuracy
# doesn't change much, it will around 0.25.
#print the output using summary() function.

glm_model <- glm(booking_status ~ 
                   no_of_special_requests+arrival_year+
                   avg_price_per_room+market_segment_type+
                   repeated_guest+no_of_previous_bookings_not_canceled+
                   no_of_adults+type_of_meal_plan+required_car_parking_space+
                   +no_of_week_nights+no_of_weekend_nights+no_of_previous_cancellations, 
                 data=train_data, family='binomial')
summary(glm_model)

# Perform cross-validation using cv.glm
cv_result <- cv.glm(data=train_data[,-1], glmfit=glm_model, K=5)

# Get cross-validated results
cv_result$delta # cross-validated estimates of prediction error


predicted<-predict(glm_model,test_data,type='response')

test_data<-test_data%>%
  mutate(status=ifelse(booking_status=='Canceled',1,0))%>%
  mutate(model_predicted = ifelse(predicted>=0.5,1,0))
misclass_cv <- misclass(test_data$model_predicted, test_data$status)

# Table of Misclassification:
#   actual
# predicted    0    1
# 0  834 1685
# 1 6456 1908
# 
# Misclassification Rate = 0.7480474 
# Accuray = 1- Misclassification Rate = 25%


#I test with different threshold, other than threshold =0, no significant change of accurcy.
# test_data<-test_data%>%
#   mutate(status=ifelse(booking_status=='Canceled',1,0))%>%
#   mutate(model_predicted = ifelse(predicted>=0,1,0))
# misclass_cv <- misclass(test_data$model_predicted, test_data$status)

# Table of Misclassification:
#   actual
# predicted    0    1
# 1 7290 3593
# 
# Misclassification Rate = 0.3301479 
# this is not meaningful

#ROC curve
library(ROCR)
#create a prediction object
predict<-prediction(predicted, test_data$status)

#plot ROC curve
roc<-performance(predict,'tpr','fpr')
plot(roc,colorize=T)

# calculate Area Under the Curve for this Logit Model
auc.perf <-  performance(predict, measure = "auc")
auc.perf@y.values
# [1] 0.2244139

# Use the which function along with colnames to obtain the index of the column
column_index <- match('arrival_date',colnames(data1))
column_index

glm_model <- glm(booking_status ~., data=train_data[,-1], family='binomial')
summary(glm_model)
# Perform cross-validation using cv.glm
cv_result <- cv.glm(data=train_data[,-1], glmfit=glm_model, K=5)

# Get cross-validated results
cv_result$delta # cross-validated estimates of prediction error

predicted<-predict(glm_model,test_data,type='response')

test_data<-test_data%>%
  mutate(status=ifelse(booking_status=='Canceled',1,0))%>%
  mutate(model_predicted = ifelse(predicted>=0.5,1,0))
misclass_cv <- misclass(test_data$model_predicted, test_data$status)

# Table of Misclassification:
#   actual
# predicted    0    1
# 0  783 2251
# 1 6507 1342
# 
# Misclassification Rate = 0.8047413
# Accuray = 1- Misclassification Rate = 20%

# I use all the features to build the logistic regression model, but accuracy
# decrease
