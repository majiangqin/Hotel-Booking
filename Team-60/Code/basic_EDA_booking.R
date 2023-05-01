# Clear environment
rm(list = ls())

library(ggplot2)
library(gridExtra)
library(tidyverse)
library(readr) #To read the dataset
library(tidyverse) #For data manipulation and visual analytics

library(correlationfunnel)
library(DataExplorer)

library(WVPlots)
library(ggthemes)

library(corrplot)

library(ROCR)

library(caret)
dataset2<-read.csv('hotel_bookings.csv',header=T)
head(dataset2)
View(dataset2)
set.seed(123)
attach(dataset2)
summary(dataset2)

#data clean
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

<<<<<<< HEAD
########################################################################################
=======
##########################################################################################################################
>>>>>>> 491261c401c106eca41b54867ff0c49ffbc1fc31
#see the distribution for customers for each year
barplot(table(arrival_date_year),col='blue')

#see the distribution for customers for each month
index=order(table(arrival_date_month))
index
barplot(table(arrival_date_month)[index],col='blue',cex.name=0.8,las=1,horiz=T)
table(arrival_date_month)
table(arrival_date_month)[index]
#see the cancellation status for custoers
barplot(table(is_canceled),col='blue')

#see different types of customers for book hotel(0 represent not cancelled, and 1 represent cancelled)
barplot(table(customer_type),col='blue')

#see different types of reserved room the customer made
barplot(table(reserved_room_type),col='blue')

#see which type of hotel the guest prefer
barplot(table(hotel),col='blue')
#prefer City hotel by guest

#see different types of meals preferred by customers
barplot(table(meal),col='blue')

#see the distribution for customers for lead time
barplot(table(lead_time),col='blue')

#see different types of reservation preferred by customers
barplot(table(reservation_status),col='blue')

#see different types of deposit preferred by customers
barplot(table(deposit_type),col='blue')



# create a contingency table of cancellations and reservations by month
table_by_month <- table(arrival_date_month, is_canceled)
table_by_month
table_by_month[,2]
# calculate the total number of bookings for each month
total_by_month <- apply(table_by_month, 1, sum)
total_by_month 

# set the row names for the data subset
df<-cbind(total_by_month, table_by_month)
df
# Create data
Bookings_by_month <- c(5929, 8068, 9794, 11089, 11791, 10939, 12661, 13877, 10508, 11160,6794,6780)
cancellation <- c(1807, 2696, 3149, 4524, 4677, 4535, 4742, 5239, 4116, 4246,2122,2371)
month_names <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October","November","December")

# Create bar plot
barplot(
  height = rbind(Bookings_by_month,cancellation),
  beside = TRUE,
  col = c("blue","green"),
  names.arg = month_names,
  legend.text=c("Bookings", "Canceled"),
  main = "Bookings and Cancellations by Month",
  xlab = "Month",
  ylab = "Number of Bookings/cancellations",
  las=2,
  args.legend = list(x = "topright", bty = "n")
  
)

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


sum(is.na(dataset2))
colSums(is.na(dataset2))
dataset2<-dataset2%>%
  drop_na("children")

colSums(is.na(dataset2))

sum(is.null(dataset2))

# dataset2<-dataset2[,-which(names(dataset2)=="reservation_status_date")]
View(dataset2)

# Plot the histogram
plot_histogram(dataset2,
               ggtheme = theme_bw(),
               title='Histogram Plots of Hotel Dataset Features',
               ncol=2,
               nrow=2)


# Plot the histogram
plot_histogram(dataset2,
               ggtheme = theme_minimal(),
               title='Histogram Plots of Hotel Dataset Features',
               ncol=2,
               nrow=2)

#For categorical variables, it is more suitable to plot bar charts. This can again be done by 2 ways: Using the DataExplorer function 'plot_bar()' to automatically generate them for the categorical variables. This function takes the following syntax:
plot_bar(dataset2, ggtheme = theme_minimal(),ncol=2,nrow=2)

#We can also use the handy 'plot_boxplot()' to plot all of our data feature with respect to a specified y value. In this case we wish to analyze with respect to our labels the Default feature so we apply the following:
plot_boxplot(data=dataset2,by='is_canceled',geom_boxplot_args= list("outlier.size" = -1, "coef" = 1e30))

#use ggplot
ggplot(data=dataset2,aes(hotel)) + geom_bar(aes(fill=is_canceled))

# Define the desired order of the months
month_order <- c("January", "February", "March", "April", "May", "June", 
                 "July", "August", "September", "October", "November", "December")

# Convert the arrival_date_month column to a factor variable with the desired order
dataset2$arrival_date_month <- factor(dataset2$arrival_date_month, levels = month_order)

# Create the plot with ordered months
ggplot(data = dataset2, aes(x = arrival_date_month)) + 
  geom_bar(aes(fill = is_canceled),position = "dodge",stat="identity") +
  labs(x = "Arrival Date Month", y = "Count", fill = "Canceled") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Create the plot with ordered months
ggplot(data = dataset2, aes(x = arrival_date_month, fill = is_canceled)) + 
  geom_bar(position = "dodge",stat="identity") +
  labs(x = "Arrival Date Month", y = "Count", fill = "Canceled") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#create a box plot of the adr variable with respect to the is_canceled variable in dataset2
ggplot(dataset2,aes(is_canceled,adr)) + geom_boxplot()+ scale_y_continuous(limits = c(0, max(dataset2$adr)), breaks = seq(0, max(dataset2$adr), 100))


#use a visualization of a correlation matrix plot to get a better idea of how our features relate pairwise to one another. This is an especially great tool for understating and avoid multicollinearity in linear regression models. We can easily do this using the 'PairPlot()' function from the WVPlots package.
library(WVPlots)
dim(dataset2)
PairPlot(dataset2,colnames(dataset2)[1:10],"Pairplot",point_color = "red")

library(readr)
library(tidyverse)
library(correlationfunnel)
library(DataExplorer)
library(WVPlots)
library(ggthemes)
library(corrplot)
library(ROCR)
library(caret)
library(reshape2)
library(e1071)

# plot_correlation_funnel() function creates a funnel plot that shows the strength and direction of the correlation between the response variable (is_canceled) and each predictor variable in the dataset.
# 
# The y-axis shows the sorted in descending order of the correlation coefficients.
# 
# By looking at this plot, you can identify which predictor variables are strongly correlated (either positively or negatively) with the response variable, and which ones are not significantly correlated. 
dataset2 %>%
  mutate_if(is.numeric,as.numeric)%>%
  binarize() %>% #bins the data immediately 
  correlate(is_canceled__1) %>%  #correlate just creates correlations plots 
  plot_correlation_funnel(interactive = TRUE, alpha = 0.7)



#split the data into training and test data sets
dim(dataset2)
sample<-sample(nrow(dataset2),0.7*nrow(dataset2))
length(sample)
train_data<-dataset2[sample,]
test_data<-dataset2[-sample,]

#observe the above correlation funnel plot, we will select the top 11 correlated variables(exclude country variable, assigned room type and reservatin_status) and move forward building a logistic regression model with just them using training dataset. 
#print the output using summary() function.
glm_model<-glm(is_canceled~deposit_type+previous_cancellations+lead_time+total_of_special_requests+market_segment+required_car_parking_spaces
               +booking_changes+distribution_channel+customer_type+hotel+previous_bookings_not_canceled,
               family="binomial",data=train_data,na.action=na.pass)

summary(glm_model)

# 
# Call:
#   glm(formula = is_canceled ~ deposit_type + previous_cancellations + 
#         lead_time + total_of_special_requests + market_segment + 
#         required_car_parking_spaces + booking_changes + distribution_channel + 
#         customer_type + hotel + previous_bookings_not_canceled, family = "binomial", 
#       data = train_data)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -8.4904  -0.7386  -0.4422   0.2301   6.5335  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                    -1.680e+00  2.059e-01  -8.160 3.34e-16 ***
#   deposit_typeNon Refund          5.501e+00  1.317e-01  41.765  < 2e-16 ***
#   deposit_typeRefundable          2.983e-01  2.270e-01   1.314 0.188943    
# previous_cancellations          2.804e+00  7.363e-02  38.087  < 2e-16 ***
#   lead_time                       4.364e-03  1.045e-04  41.767  < 2e-16 ***
#   total_of_special_requests      -7.217e-01  1.367e-02 -52.797  < 2e-16 ***
#   market_segmentComplementary    -7.004e-02  2.606e-01  -0.269 0.788154    
# market_segmentCorporate        -4.186e-01  2.041e-01  -2.051 0.040283 *  
#   market_segmentDirect           -3.489e-02  2.260e-01  -0.154 0.877331    
# market_segmentGroups           -2.625e-01  2.138e-01  -1.227 0.219704    
# market_segmentOffline TA/TO    -7.937e-01  2.145e-01  -3.701 0.000215 ***
#   market_segmentOnline TA         7.263e-01  2.134e-01   3.404 0.000664 ***
#   required_car_parking_spaces    -1.645e+01  5.016e+01  -0.328 0.742891    
# booking_changes                -3.753e-01  1.808e-02 -20.755  < 2e-16 ***
#   distribution_channelDirect     -4.046e-01  1.100e-01  -3.677 0.000236 ***
#   distribution_channelGDS        -9.004e-01  2.342e-01  -3.844 0.000121 ***
#   distribution_channelTA/TO       2.640e-02  8.409e-02   0.314 0.753593    
# distribution_channelUndefined   7.352e-01  3.956e+03   0.000 0.999852    
# customer_typeGroup             -2.438e-01  1.963e-01  -1.242 0.214196    
# customer_typeTransient          7.910e-01  6.351e-02  12.455  < 2e-16 ***
#   customer_typeTransient-Party    2.423e-01  6.713e-02   3.610 0.000306 ***
#   hotelResort Hotel               7.199e-02  1.993e-02   3.612 0.000304 ***
#   previous_bookings_not_canceled -6.469e-01  3.304e-02 -19.579  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 109857  on 83227  degrees of freedom
# Residual deviance:  72558  on 83205  degrees of freedom
# AIC: 72604
# 
# Number of Fisher Scoring iterations: 16


#predict with test data set using 0.5 as cutoff 
test_data$model_pro<-predict(glm_model,test_data,type="response")
head(test_data)

df<-test_data%>%
  mutate(model_predict=ifelse(model_pro>=0.5,1,0))
df<-df%>%
  mutate(model_predict=as.factor(model_predict))
head(df)

#plot model predictions resulting from applying our threshold to see how many canceled and non-canceled our model predicted.
ggplot(data=df)+geom_bar(aes(model_predict))

#make confusion matrix
xtabs(~is_canceled+model_predict,data=df)

# model_predict
# is_canceled     0     1
# 0 21019  1453
# 1  5334  7864

draw_confusion_matrix <- function(cm) {
  
  layout(matrix(c(1,1,2)))
  par(mar=c(2,2,2,2))
  plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  title('CONFUSION MATRIX', cex.main=2)
  
  # create the matrix 
  rect(150, 430, 240, 370, col='#3F97D0')
  text(195, 435, '0', cex=1.2)
  rect(250, 430, 340, 370, col='#F7AD50')
  text(295, 435, '1', cex=1.2)
  text(125, 370, 'Predicted', cex=1.3, srt=90, font=2)
  text(245, 450, 'Actual', cex=1.3, font=2)
  rect(150, 305, 240, 365, col='#F7AD50')
  rect(250, 305, 340, 365, col='#3F97D0')
  text(140, 400, '0', cex=1.2, srt=90)
  text(140, 335, '1', cex=1.2, srt=90)
  
  # add in the cm results 
  res <- as.numeric(cm$table)
  text(195, 400, res[1], cex=1.6, font=2, col='white')
  text(195, 335, res[2], cex=1.6, font=2, col='white')
  text(295, 400, res[3], cex=1.6, font=2, col='white')
  text(295, 335, res[4], cex=1.6, font=2, col='white')
  
  # add in the specifics 
  plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", main = "DETAILS", xaxt='n', yaxt='n')
  text(10, 85, names(cm$byClass[1]), cex=1.2, font=2)
  text(10, 70, round(as.numeric(cm$byClass[1]), 3), cex=1.2)
  text(30, 85, names(cm$byClass[2]), cex=1.2, font=2)
  text(30, 70, round(as.numeric(cm$byClass[2]), 3), cex=1.2)
  text(50, 85, names(cm$byClass[5]), cex=1.2, font=2)
  text(50, 70, round(as.numeric(cm$byClass[5]), 3), cex=1.2)
  text(70, 85, names(cm$byClass[6]), cex=1.2, font=2)
  text(70, 70, round(as.numeric(cm$byClass[6]), 3), cex=1.2)
  text(90, 85, names(cm$byClass[7]), cex=1.2, font=2)
  text(90, 70, round(as.numeric(cm$byClass[7]), 3), cex=1.2)
  
  # add in the accuracy information 
  text(30, 35, names(cm$overall[1]), cex=1.5, font=2)
  text(30, 20, round(as.numeric(cm$overall[1]), 3), cex=1.4)
  text(70, 35, names(cm$overall[2]), cex=1.5, font=2)
  text(70, 20, round(as.numeric(cm$overall[2]), 3), cex=1.4)
} 

#With our function defined we now apply it and plot the confusion matrix. As well as the standard printed caret library summary of confusion matrix data using the 'confusionMatrix()' function.
Confusion_mat = caret::confusionMatrix(df$model_predict, test_data$is_canceled,positive='1')
Confusion_mat
# Confusion Matrix and Statistics
# 
# Reference
# Prediction     0     1
# 0 21019  5334
# 1  1453  7864
# 
# Accuracy : 0.8097          
# 95% CI : (0.8056, 0.8138)
# No Information Rate : 0.63            
# P-Value [Acc > NIR] : < 2.2e-16       
# 
# Kappa : 0.5655          
# 
# Mcnemar's Test P-Value : < 2.2e-16       
#                                           
#             Sensitivity : 0.5958          
#             Specificity : 0.9353          
#          Pos Pred Value : 0.8440          
#          Neg Pred Value : 0.7976          
#              Prevalence : 0.3700          
#          Detection Rate : 0.2205          
#    Detection Prevalence : 0.2612          
#       Balanced Accuracy : 0.7656          
#                                           
#        'Positive' Class : 1 

draw_confusion_matrix(Confusion_mat)


## Visualizing ROC Curve:
#Now we can calculate the AUC, which is often a better measure than accuracy alone, particularly when considering imbalanced data. 
#The ROCR package assists in this. Firstly, we create a prediction object using our model and data not our previously solved for static preidctions. 
#The predicition object will allow the computation of ROC statistics across a spectrum of varying threshold values. Then we created an ROC object from which we could calculate the AUC.

predicts <- prediction(as.numeric(df$model_pro),as.numeric(df$is_canceled))
roc <- performance(predicts,"tpr", "fpr")
plot(roc,main="ROC curve for GLM model")
auc_ROCR <- performance(predicts, measure = "auc")
auc_ROCR <- auc_ROCR@y.values[[1]]
print(paste("AUC value for logistic regression: ",auc_ROCR))
#[1] "AUC value for logistic regression:  0.84705304126577"


#################################################################################################################################################################
#time series 

#for each hotel type, show the number of cancelled and not cancelled
dataset2%>%
  group_by(hotel)%>%
  select(is_canceled)%>%
  table()
# is_canceled
# hotel              0     1
# City Hotel   46226 33076
# Resort Hotel 28519 11077

#count the number of observations in each hotel type
dataset2%>%
  group_by(hotel)%>%
  select(is_canceled)%>%
  tally()

# hotel            n
# <fct>        <int>
#   1 City Hotel   79302
# 2 Resort Hotel 39596



# training<-dataset2%>%
#   sample_frac(0.7,replace=FALSE)
# 
# test <- dataset2 %>% 
#   anti_join(training)
# test
# dim(training)
# dim(test)
dim(dataset2)

#separate the dataset for the hotel type
# View(dataset2)
# resort<-dataset2%>%slice(1:40061)%>%select(reservation_status_date,is_canceled)
# city<-dataset2%>%slice(40062:119391)%>%select(reservation_status_date,is_canceled)

resort<-dataset2 %>%
  filter(hotel=='City Hotel') %>%
  select(reservation_status_date,is_canceled)

city<-dataset2 %>%
  filter(hotel=='Resort Hotel') %>%
  select(reservation_status_date,is_canceled)

#calculate cancel ratio for each month for resort hotel
resort_monthly <-resort %>%
  mutate(data=as.Date(reservation_status_date)) %>%
  mutate(year_month = format(data,'%Y-%m')) %>%
  group_by(year_month) %>%
  summarise(total_reservations = n(),
            canceled_reservations = sum(as.numeric(as.character(is_canceled))),
            cancel_ratio=canceled_reservations/total_reservations)

resort_monthly<-resort_monthly%>%
  slice(-1,) %>%
  select(year_month,cancel_ratio)

resort_monthly


#calculate cancel ratio for each month for city hotel
city_monthly<-city %>%
  mutate(date=as.Date(reservation_status_date)) %>%
  mutate(year_month=format(date,"%Y-%m")) %>%
  group_by(year_month) %>%
  summarise(total_reservations = n(),
            canceled_reservations = sum(as.numeric(as.character(is_canceled))),
            cancel_ratio=canceled_reservations/total_reservations
  )

city_monthly<-city_monthly %>%
  slice(-1,) %>%
  select(year_month,cancel_ratio)

# # create vectors of the two data
# resort <- as.vector(unlist(resort_monthly))
# resort
# city <- as.vector(unlist(city))
# city

# turn the vector into a time series object
myts_resort <- ts(resort_monthly$cancel_ratio,start=c(2015,1),frequency=12)
myts_resort

myts_city <- ts(city_monthly$cancel_ratio,start=c(2015,1),frequency = 12)
myts_city

#decompose” the time series — which in this case means separating out the 3 main components that make up the time series:

#trend: the long-term trends in the data
# seasonal: the repeated seasonal signal adder
# random: the “left-over” components that aren’t expected from the seasonality or trend components.
# We can easily extract these components and plot them with:

components_dfts_resort <- decompose(myts_resort)
plot(components_dfts_resort)
# from the plots we can see that the cancel ration has decrease trend, and has a 
# may repeated seasonal effect for resort hotel

components_dfts_city <- decompose(myts_city)
plot(components_dfts_city)
# from the plots we can see that the cancel ration has decrease trend, and has a 
# may repeated seasonal effect for city hotel


# Triple exponential smoothing (additive seasonality) for resort and city hotels
# The standard Holt-Winters fits use an additive seasonality 
# — where it assumes that the amplitude of any seasonality components 
# are relatively constant throughout the series.

HW1 <- HoltWinters(myts_resort)
HW1
# Holt-Winters exponential smoothing with trend and additive seasonal component.
# 
# Call:
#   HoltWinters(x = myts_resort)
# 
# Smoothing parameters:
#   alpha: 0.5396641
# beta : 0
# gamma: 1
# 
# Coefficients:
#   [,1]
# a    0.148349654
# b   -0.031920321
# s1  -0.109842787
# s2  -0.009263268
# s3   0.120690563
# s4   0.131773995
# s5   0.104366014
# s6   0.024070764
# s7   0.056325375
# s8   0.006550166
# s9  -0.047973185
# s10  0.008277293
# s11 -0.064987603
# s12 -0.148349654
plot(HW1)

HW2<- HoltWinters(myts_city)
HW2
# Holt-Winters exponential smoothing with trend and additive seasonal component.
# 
# Call:
#   HoltWinters(x = myts_city)
# 
# Smoothing parameters:
#   alpha: 0.7107867
# beta : 0
# gamma: 1
# 
# Coefficients:
#   [,1]
# a    0.21737606
# b   -0.02838971
# s1  -0.14866024
# s2  -0.04750798
# s3   0.09214078
# s4   0.13457231
# s5   0.11720920
# s6   0.12543977
# s7   0.08201687
# s8   0.05855111
# s9   0.11702991
# s10 -0.03131647
# s11 -0.17428966
# s12 -0.21737606
plot(HW2)

#use multiplicative decomposition, this method is not suit
# HW11 <- HoltWinters(myts_resort,seasonal = 'multiplicative')
# HW11
# plot(HW11)
# 
# HW22<- HoltWinters(myts_city,seasonal = 'multiplicative')
# HW22
# plot(HW22)



library(forecast)
# we can use forecast to make new predictions for one year
# and include both 80% and 95% confidence intervals

HW1_for <- forecast(HW1, h=12, level=c(80,95))
HW2_for <- forecast(HW2, h=12, level=c(80,95))

#visualize our predictions:
plot(HW1_for, xlim=c(2015, 2019))
lines(HW1_for$fitted, lty=2, col="purple")


plot(HW2_for, xlim=c(2015, 2019))
lines(HW2_for$fitted, lty=2, col="purple")

acf(HW1_for$residuals, lag.max=20, na.action=na.pass)
Box.test(HW1_for$residuals, lag=20, type="Ljung-Box")
hist(HW1_for$residuals)

acf(HW2_for$residuals, lag.max=20, na.action=na.pass)
Box.test(HW2_for$residuals, lag=20, type="Ljung-Box")
hist(HW2_for$residuals)




# Double exponential smoothing
m2 <- HoltWinters(myts_resort,gamma=FALSE)
m2
plot(m2)
# Holt-Winters exponential smoothing with trend and without seasonal component.
# 
# Call:
#   HoltWinters(x = myts_resort, gamma = FALSE)
# 
# Smoothing parameters:
#   alpha: 1
# beta : 0.0243745
# gamma: FALSE
# 
# Coefficients:
#   [,1]
# a  0.00000000
# b -0.01733637


#not seansonal component
m2_city <- HoltWinters(myts_city,gamma=FALSE)
m2_city

# Holt-Winters exponential smoothing with trend and without seasonal component.
# 
# Call:
#   HoltWinters(x = myts_city, gamma = FALSE)
# 
# Smoothing parameters:
#   alpha: 1
# beta : 0.01161175
# gamma: FALSE
# 
# Coefficients:
#   [,1]
# a  0.000000000
# b -0.009358984


plot(m2_city)
m2_for <- forecast(m2, h=12, level=c(80,95))
m2_for_city <- forecast(m2_city, h=12, level=c(80,95))


#visualize our predictions:
plot(m2_for, xlim=c(2015, 2019))
lines(m2_for$fitted, lty=2, col="purple")

plot(m2_for_city, xlim=c(2015, 2019))
lines(m2_for_city$fitted, lty=2, col="purple")

acf(m2_for$residuals, lag.max=20, na.action=na.pass)
Box.test(m2_for$residuals, lag=20, type="Ljung-Box")
hist(m2_for$residuals)


# Box-Ljung test
# 
# data:  m2_for$residuals
# X-squared = 17.45, df = 20, p-value = 0.6236

acf(m2_for_city$residuals, lag.max=20, na.action=na.pass)
Box.test(m2_for_city$residuals, lag=20, type="Ljung-Box")
hist(m2_for_city$residuals)

# Box-Ljung test
# 
# data:  m2_for_city$residuals
# X-squared = 7.1225, df = 20, p-value = 0.9963



# In this case, the p-value is 0.6236 and 0.9963, which is greater than the commonly
# used significance level of 0.05, indicating that there is no evidence of 
# significant autocorrelation in the residuals at the 20 lags considered in 
# the test.
#also the ACF values are mostly within the confidence bands (usually denoted by
# dotted lines), it indicates that there is no significant autocorrelation
# in the residuals.
#and plot of residuals is close to normal distribution, which is good.
