
rm(list=ls())
library(tidyverse)
dataset2<-read.csv('hotel_bookings.csv',header=T)
head(dataset2)
set.seed(123)
attach(dataset2)
summary(dataset2)
################################################################################
#Data clean

#Converting categorical variables to factors
dataset2$hotel <- as.factor(dataset2$hotel)
dataset2$arrival_date_year <- as.factor(dataset2$arrival_date_year)
dataset2$arrival_date_month <- as.factor(dataset2$arrival_date_month)
dataset2$meal <- as.factor(dataset2$meal)
dataset2$country <- as.factor(dataset2$country)
dataset2$market_segment <- as.factor(dataset2$market_segment)
dataset2$distribution_channel <- as.factor(dataset2$distribution_channel)
dataset2$reserved_room_type <- as.factor(dataset2$reserved_room_type)
dataset2$assigned_room_type <- as.factor(dataset2$assigned_room_type)
dataset2$customer_type <- as.factor(dataset2$customer_type)
dataset2$deposit_type <- as.factor(dataset2$deposit_type)
dataset2$is_canceled<-as.factor(dataset2$is_canceled)
dataset2$reservation_status <-as.factor(dataset2$reservation_status)

#check which column has NULL values
null_cols <- sapply(dataset2, function(x) any(grepl("NULL", x, ignore.case = TRUE)))
HotelData_null <- dataset2[, null_cols]
HotelData_null

#remove agent and company columns, which has many NULL values
dataset2<-dataset2[,-which(names(dataset2)=="agent")]
dataset2<-dataset2[,-which(names(dataset2)=="company")]
dataset2

#remove NULL value rows in country column
dataset2<-dataset2[country!='NULL',]
dataset2

null_cols <- sapply(dataset2, function(x) any(grepl("NULL", x, ignore.case = TRUE)))
HotelData_null <- dataset2[, null_cols]
HotelData_null

#remove NA values
sum(is.na(dataset2))
colSums(is.na(dataset2))
dataset2<-dataset2%>%
  drop_na("children")

#check NA/NULL values
colSums(is.na(dataset2))
sum(is.null(dataset2))

# create a is_Domestic variable
sum(dataset2$country == "PRT")
sum(dataset2$country != "PRT")
dataset2$is_Domestic[dataset2$country != "PRT"] <- "0"
dataset2$is_Domestic[dataset2$country == "PRT"] <- "1"
sum(dataset2$is_Domestic == "0")
sum(dataset2$is_Domestic == "1")


#features selection part

# I decide to  do feature selection, which is a common technique used
# to reduce the complexity of the model and improve its performance.
# By doing so, we can potentially reduce the training time of the model
# and improve its interpretability.


# plot_correlation_funnel() function creates a funnel plot that shows
#the strength and direction of the correlation between the response variable
#(is_canceled) and each predictor variable in the dataset.
# The y-axis shows the sorted in descending order of the correlation coefficients.
# By looking at this plot, we can identify which predictor variables are
# strongly correlated (either positively or negatively) with the response
# variable, and which ones are not significantly correlated.

library(correlationfunnel)
library(corrplot)

dataset2 %>%
  mutate_if(is.numeric,as.numeric)%>%
  binarize() %>% #bins the data immediately 
  correlate(is_canceled__1) %>%  #correlate just creates correlations plots 
  plot_correlation_funnel(interactive = TRUE, alpha = 0.7)

# observe the above correlation funnel plot, we will select some significant
# variables and move forward building models with just them using training dataset.
# the chosen variables are deposit_type, previous_cancellations, lead_time,
# total_of_special_requests, market_segment, required_car_parking_spaces
# booking_changes,is_Domestic, customer_type, and hotel

dim(dataset2)
set.seed(1234)
dataset<-sample(nrow(dataset2),0.2*nrow(dataset2))
length(dataset)
dataset<-dataset2[dataset,]
dim(dataset)

#After data clean, this dataset has 118898 observations, which is too large.
# It takes very long time to tuning the models,using a sub-sampling ratio of
# 20% would result in a sub dataset of approximately 23779 observations. 
# While this can be a reasonable approach for managing large datasets

# I split the hotel reservation dataset into training and test datasets,70%
# for trainng, and 30% for test,
# using training dataset for cross validation of svm, knn and logistic 
# regression models, and using test dataset for compare their performance.
# using a training dataset for cross-validation and a separate test dataset
# for comparing the performance of different models is a common and 
# recommended approach in machine learning and model evaluation. 
# This ensures that the models are evaluated on unseen data, which provides
# a more reliable assessment of their generalization performance.
set.seed(1234)
sample<-sample(nrow(dataset),0.7*nrow(dataset))
length(sample)
train_data<-dataset[sample,]
test_data<-dataset[-sample,]

# defines a function called misclass that calculates the misclassification
# rate given predicted and actual values.
misclass<- function (predicted,actual){
  tbl=table(predicted,actual)
  missclass_rate=1-sum(diag(tbl))/sum(tbl)
  cat('Table of Misclassification: \n')
  print(tbl)
  cat("The Misclassfication Rate = ", missclass_rate,"\n")
  return(missclass_rate)
}



#######################################################################################
#SVM model

library(kernlab)
set.seed(123)

#tuning the SVM model to find optimal C and sigma

# Define the tuning parameters
# I also tune the sigma and C value manually, after several times tuning,
# identify combination values at smaller range as follows, 
# the actual tuning time is more than two hours.

tune_grid <- expand.grid(sigma = c(0.1, 1, 10),
                         C = c(10, 100, 1000))

# initialize variables to store the minimum misclassification rate and corresponding C and sigma values
min_error <- Inf
min_C <- NA
min_sigma <- NA

#to calculate the time the tuning SVM model used
# start the timer
start_time <- Sys.time()


#be careful, the hyperparameters tuning part will take a long time, maybe several hours,
#you may skip this part, just run the part of create SVM model with optimal C 
#and sigma values, it will start from line 203

#loop over each combination of C and sigma in tune_grid, to find the optimal
# C and sigma with the lowest misclassifiction rate.
for (i in 1:nrow(tune_grid)) {
  sigma <- tune_grid$sigma[i]
  C <- tune_grid$C[i]
  ksvm.tuned = ksvm(is_canceled ~ deposit_type + previous_cancellations + 
                      lead_time + total_of_special_requests + 
                      market_segment +required_car_parking_spaces + booking_changes +
                      is_Domestic + customer_type,
                    data=as.data.frame(train_data),
                    kpar=list(sigma=sigma),C=C,cross=5,type = "C-svc",scaled = T,
                    kernel="rbfdot")
  yhat = predict(ksvm.tuned,newdata=train_data)
  
  # compute the misclassification rate
  error <- misclass(yhat,train_data$is_canceled)
  
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
#Elapsed time: 1.264343 hours

# print the minimum misclassification rate and corresponding C and sigma values
cat("Minimum misclassification rate:", min_error, "\n")
cat("Corresponding C value:", min_C, "\n")
cat("Corresponding sigma value:", min_sigma, "\n")

# Minimum misclassification rate: 0.1428657 
# Corresponding C value: 1000
# Corresponding sigma value: 10


# start the timer
#it will take about half an hour, it depends on your computer's memory.
start_time <- Sys.time()
#create SVM model with optimal C and sigma values
ksvm.tuned = ksvm(is_canceled ~ deposit_type + previous_cancellations + 
                    lead_time + total_of_special_requests + 
                    market_segment +required_car_parking_spaces + booking_changes +
                    is_Domestic + customer_type,
                  data=as.data.frame(train_data),
                  kpar=list(sigma=10),C=1000,cross=5,type = "C-svc",scaled = T,
                  kernel="rbfdot")

# stop the timer
end_time <- Sys.time()

# calculate the elapsed time
elapsed_time <- end_time - start_time

# print the elapsed time
cat("Elapsed time:", elapsed_time, "\n")
#Elapsed time: 34.29967 minutes

#see the SVM model characteristics
ksvm.tuned
# Support Vector Machine object of class "ksvm" 
# 
# SV type: C-svc  (classification) 
# parameter : cost C = 1000 
# 
# Gaussian Radial Basis kernel function. 
# Hyperparameter : sigma =  10 
# 
# Number of Support Vectors : 7860 
# 
# Objective Function Value : -4883798 
# Training error : 0.142866 
# Cross validation error : 0.181616 

# compute the misclassification rate with test dataset
yhat = predict(ksvm.tuned,newdata=test_data)
misclassification_rate <- misclass(yhat,test_data$is_canceled)


library(caret)
#printed caret library summary of confusion matrix data 
Confusion_mat1 = caret::confusionMatrix(yhat, test_data$is_canceled,positive='1')
Confusion_mat1

# Confusion Matrix and Statistics
# 
# Reference
# Prediction    0    1
# 0 4055  701
# 1  498 1880
# 
# Accuracy : 0.8319         
# 95% CI : (0.823, 0.8405)
# No Information Rate : 0.6382         
# P-Value [Acc > NIR] : < 2.2e-16      
# 
# Kappa : 0.6297         
# 
# Mcnemar's Test P-Value : 5.422e-09      
#                                          
#             Sensitivity : 0.7284         
#             Specificity : 0.8906         
#          Pos Pred Value : 0.7906         
#          Neg Pred Value : 0.8526         
#              Prevalence : 0.3618         
#          Detection Rate : 0.2635         
#    Detection Prevalence : 0.3333         
#       Balanced Accuracy : 0.8095         
#                                          
#        'Positive' Class : 1 


################################################################################
#KNN model
set.seed(1)
library(kknn)

# set maximum value of k (number of neighbors) to test
kmax <- 50

#create a array of misclassification rates for different k
misclass_cv<-rep(0,kmax)


# start the timer
start_time <- Sys.time()

# calculate misclassification rates for different k
for(k in 1:kmax){
  knn.tuned=cv.kknn(is_canceled ~ deposit_type + previous_cancellations + 
                      lead_time + total_of_special_requests + 
                      market_segment +required_car_parking_spaces + booking_changes +
                      is_Domestic + customer_type,
                    data=train_data,
                    k=k,
                    scale=T,
                    kcv=5)
  predic=as.integer(knn.tuned[[1]][, 2] + 0.5)
  misclass_cv[k]=misclass(predic,train_data$is_canceled)
  
}

# stop the timer
end_time <- Sys.time()

# calculate the elapsed time
elapsed_time <- end_time - start_time

# print the elapsed time
cat("Elapsed time:", elapsed_time, "\n")
#Elapsed time: 3.94755 minutes

# show misclassification rates
misclass_cv

min(misclass_cv)

which.min(misclass_cv)

data.frame(k = 1:length(misclass_cv), misclassification = misclass_cv) %>%
  ggplot(aes(x = k, y = misclassification)) +
  geom_line() +
  xlab("Number of neighbors (k)") +
  ylab("misclassification rates")

# start the timer
start_time <- Sys.time()

#create KNN mode with optimal k=48
kknn.tuned <- kknn(is_canceled ~ deposit_type + previous_cancellations + 
                     lead_time + total_of_special_requests + 
                     market_segment +required_car_parking_spaces + booking_changes +
                     is_Domestic + customer_type,
                   train=train_data, 
                   test=test_data, k = 45, scale = TRUE)
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
misclass_cv <- misclass(predicted, test_data$is_canceled)

#printed caret library summary of confusion matrix data 
Confusion_mat2 = caret::confusionMatrix(predicted, test_data$is_canceled,positive='1')
Confusion_mat2

# Confusion Matrix and Statistics
# 
# Reference
# Prediction    0    1
# 0 4121  731
# 1  432 1850
# 
# Accuracy : 0.837           
# 95% CI : (0.8282, 0.8455)
# No Information Rate : 0.6382          
# P-Value [Acc > NIR] : < 2.2e-16       
# 
# Kappa : 0.6379          
# 
# Mcnemar's Test P-Value : < 2.2e-16       
#                                           
#             Sensitivity : 0.7168          
#             Specificity : 0.9051          
#          Pos Pred Value : 0.8107          
#          Neg Pred Value : 0.8493          
#              Prevalence : 0.3618          
#          Detection Rate : 0.2593          
#    Detection Prevalence : 0.3199          
#       Balanced Accuracy : 0.8109          
#                                           
#        'Positive' Class : 1  

#######################################################################################
#logistic regression model

set.seed(1)
glm_model = glm(is_canceled ~ deposit_type + previous_cancellations + 
                  lead_time + total_of_special_requests + 
                  market_segment +required_car_parking_spaces + booking_changes +
                  is_Domestic + customer_type, data=train_data,
                family='binomial')
summary(glm_model)

# Call:
#   glm(formula = is_canceled ~ deposit_type + previous_cancellations + 
#         lead_time + total_of_special_requests + market_segment + 
#         required_car_parking_spaces + booking_changes + is_Domestic + 
#         customer_type, family = "binomial", data = train_data)

# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -4.1163  -0.7239  -0.3503   0.2468   2.8983  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                  -3.290e+00  5.390e-01  -6.104 1.03e-09 ***
#   deposit_typeNon Refund        4.928e+00  2.892e-01  17.038  < 2e-16 ***
#   deposit_typeRefundable        8.016e-01  5.942e-01   1.349    0.177    
# previous_cancellations        6.727e-01  9.064e-02   7.422 1.16e-13 ***
#   lead_time                     5.571e-03  2.462e-04  22.627  < 2e-16 ***
#   total_of_special_requests    -7.409e-01  3.129e-02 -23.680  < 2e-16 ***
#   market_segmentComplementary  -8.081e-01  6.288e-01  -1.285    0.199    
# market_segmentCorporate      -5.859e-01  5.318e-01  -1.102    0.271    
# market_segmentDirect          2.958e-01  5.225e-01   0.566    0.571    
# market_segmentGroups          5.616e-01  5.261e-01   1.067    0.286    
# market_segmentOffline TA/TO   3.948e-02  5.217e-01   0.076    0.940    
# market_segmentOnline TA       2.019e+00  5.188e-01   3.892 9.94e-05 ***
#   required_car_parking_spaces  -1.673e+01  1.124e+02  -0.149    0.882    
# booking_changes              -3.859e-01  4.218e-02  -9.149  < 2e-16 ***
#   is_Domestic1                  1.545e+00  5.049e-02  30.604  < 2e-16 ***
#   customer_typeGroup            1.561e-01  3.754e-01   0.416    0.678    
# customer_typeTransient        7.924e-01  1.467e-01   5.400 6.67e-08 ***
#   customer_typeTransient-Party  1.101e-01  1.544e-01   0.713    0.476    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 21985  on 16644  degrees of freedom
# Residual deviance: 13763  on 16627  degrees of freedom
# AIC: 13799
# 
# Number of Fisher Scoring iterations: 16



#predict with test dataset using 0.5 as cutoff 
test_data$predict_percent<-predict(glm_model,test_data,type='response')

test_data<-test_data %>%
  mutate(predict_model=ifelse(predict_percent>=0.5,1,0))

test_data$predict_model<-as.factor(test_data$predict_model)

test_data

#make confusion matrix
xtabs(~is_canceled+predict_model,data=test_data)
#or use misclass function
misclass(test_data$is_canceled,test_data$predict_model)


#printed caret library summary of confusion matrix data 
Confusion_mat3 = caret::confusionMatrix(test_data$predict_model, test_data$is_canceled,positive='1')
Confusion_mat3

#Confusion Matrix and Statistics
# 
# Reference
# Prediction    0    1
# 0 4166 1033
# 1  387 1548
# 
# Accuracy : 0.801           
# 95% CI : (0.7915, 0.8102)
# No Information Rate : 0.6382          
# P-Value [Acc > NIR] : < 2.2e-16       
# 
# Kappa : 0.5443          
# 
# Mcnemar's Test P-Value : < 2.2e-16       
#                                           
#             Sensitivity : 0.5998          
#             Specificity : 0.9150          
#          Pos Pred Value : 0.8000          
#          Neg Pred Value : 0.8013          
#              Prevalence : 0.3618          
#          Detection Rate : 0.2170          
#    Detection Prevalence : 0.2712          
#       Balanced Accuracy : 0.7574          
#                                           
#        'Positive' Class : 1  