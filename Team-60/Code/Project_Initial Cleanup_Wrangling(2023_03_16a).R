#Load libraries
library(readr)
library(lubridate)
library(tidyverse)
library(scatterplot3d)
library(ggplot2)
library(viridis)
library(ggExtra)
library(ROCR)
library(GGally)
library(kernlab)
library(dplyr)
library(Hmisc)
library(glmnet)
library(factoextra)
library(outliers)
library(corrplot)
library(caret)
library(kernlab)
library(magrittr)
library(tidyr)

rm(list = ls())
set.seed(123)
#Import data
HotelData_0 <- data.frame(read_csv('hotel_bookings.csv'),stringsAsFactors=FALSE)
HotelData_0 %>% relocate(is_canceled)

HotelReservations <- data.frame(read_csv('C:\\Users\\nsous\\OneDrive\\Desktop\\Project\\Data Files\\HotelReservations.csv'),stringsAsFactors=FALSE)

#HotelData_0 <- data.frame(read_csv('https://www.kaggle.com/datasets/jessemostipak/hotel-booking-demand?resource=download&select=hotel_bookings.csv'))
head(HotelData_0)
#notice some data points have adult = 0 
str(HotelData_0)

# The 'arrival weekday' attribute was added by data set creators

# DATA EXPLORATION AND CLEAN-UP TASKS
# Stays_in_Weekend_nights is Number of weekend nights (Saturday or Sunday) the guest booked
# (counting the number of weekend nights from total number of nights)
# Stays_in_week_nights is number of week nights (Mon thru Friday) the guest booked
# CHECK TO SEE IF ANY DATA POINT HAS A ZERO VALUE FOR BOTH
sum(HotelData_0$stays_in_weekend_nights == "0" & HotelData_0$stays_in_week_nights== "0")
# THERE ARE 715 DATAPOINTS THAT HAVE ZERO VALUES FOR BOTH VARIABLES!
# WE'LL EITHER DERIVE THESE VALUES OR REMOVE THESE DATA POINTS

#Verify if any NA values exist that may needed to be mutated or removed
sum(is.na(HotelData_0))
#total of 4 NA values

#which column(s) have NA values?
colnames(HotelData_0)[ apply(HotelData_0, 2, anyNA)]
# Only children

#Verify Children has 4 NA values
table(is.na(HotelData_0$children))
# Yes 4.  We remove these data points.
HotelData_0 <- HotelData_0[!(is.na(HotelData_0$children)),]

#Verify if an NULL (empty) values exist that may needed to be mutated or removed
sum(is.null(HotelData_0))
#none

#Verify which columns have a value of "NULL"
colnames(HotelData_0)[which(HotelData_0 == "NULL", arr.ind=T)[, "col"]]
#agent and country use null values; we will not use agent in our model.
sum(HotelData_0$country == "NULL")
# 488 Data points with country code = Null. If we regress on country then we should remove.

#Verify if there are reservations with adults equal to zero
sum(HotelData_0$adults == "0")
#there are 403 data points with adult equal to zero. SHOULD BE REMOVED.
HotelData_1a <- HotelData_0[(HotelData_0$adults) >= 1,]
#Verify if successful
sum(HotelData_1a$adults == "0")

sum(HotelData_1a$stays_in_weekend_nights == "0" & HotelData_1a$stays_in_week_nights == "0")
### REMOVE 645 DATA POINTS WITH WEEKEND AND WEEKNIGHT BOTH = 0
HotelData_1 <- subset(HotelData_1a, HotelData_1a$stays_in_weekend_nights != "0" | HotelData_1a$stays_in_week_nights != "0")

#The data introduction document noted meal plan values of 'undefined' and "SC' are equivalent
#Identify count of 'Undefined' and  'SC'
table(HotelData_1$meal)
#there 1169 data points with a value of undefined.  We'll changes these to "SC"
HotelData_1$meal[HotelData_1$meal=="Undefined"]<-"SC"
#Verify there are no more 'Undefined' and  'SC' has increased by 1169
table(HotelData_1$meal)
#successful
str(HotelData_1)

# Using Linear Regression on numerical variables to Model Binary Outcomes (just for perspective)
a.lm <- lm(formula = is_canceled ~ lead_time + stays_in_weekend_nights + stays_in_week_nights + adults +
             children + babies + is_repeated_guest + previous_cancellations + previous_bookings_not_canceled +
             booking_changes + adr + total_of_special_requests, data = HotelData_1)
summary(a.lm)

# create a is_Domestic variable
HotelData_2 <- HotelData_1
sum(HotelData_2$country == "PRT")
sum(HotelData_2$country != "PRT")
HotelData_2$is_Domestic[HotelData_2$country != "PRT"] <- "0"
HotelData_2$is_Domestic[HotelData_2$country == "PRT"] <- "1"
sum(HotelData_2$is_Domestic == "0")
sum(HotelData_2$is_Domestic == "1")

#convert categorical variables to factors
HotelData_2$hotel <- as.factor(HotelData_2$hotel)
HotelData_2$arrival_date_year <- as.factor(HotelData_2$arrival_date_year)
HotelData_2$arrival_date_month <- as.factor(HotelData_2$arrival_date_month)
HotelData_2$meal <- as.factor(HotelData_2$meal)
HotelData_2$country <- as.factor(HotelData_2$country)
HotelData_2$market_segment <- as.factor(HotelData_2$market_segment)
HotelData_2$distribution_channel <- as.factor(HotelData_2$distribution_channel)
HotelData_2$reserved_room_type <- as.factor(HotelData_2$reserved_room_type)
HotelData_2$assigned_room_type <- as.factor(HotelData_2$assigned_room_type)
HotelData_2$customer_type <- as.factor(HotelData_2$customer_type)
HotelData_2$deposit_type <- as.factor(HotelData_2$deposit_type)
HotelData_2$is_Domestic <- as.factor(HotelData_2$is_Domestic)

# Scatter Plot + Linear Regression line of Canceled vs. lead time + Logistic Model Curve
ggplot(HotelData_2, aes(x=lead_time, y=is_canceled)) +
  geom_point() + 
  theme(axis.text.x = element_text(size=20), axis.text.y = element_text(size=20), 
        axis.title=element_text(size=20,face="bold")) + 
  # add logit curve
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE) +
  #add the regression line
  geom_smooth(method=lm,  color="red", # Add linear regression lines
              se=FALSE,    # Don't add shaded confidence region
              fullrange=TRUE) 

HotelData_2$is_canceled <- as.factor(HotelData_2$is_canceled)

str(HotelData_2)

#Exploratory logistic model
Logmodel1 <- glm(formula = is_canceled ~ lead_time + arrival_date_year + arrival_date_month + is_Domestic +
                   stays_in_weekend_nights + stays_in_week_nights + adults + children + babies + meal + market_segment +
                   distribution_channel + is_repeated_guest + previous_cancellations + previous_bookings_not_canceled +
                   reserved_room_type + booking_changes + deposit_type + customer_type + adr + total_of_special_requests,
                 data = HotelData_2,family = "binomial")
summary(Logmodel1)

# create a Season variable
HotelData_3 <- HotelData_2
HotelData_3$Season[HotelData_3$arrival_date_month == "December"|HotelData_3$arrival_date_month == "January" |HotelData_3$arrival_date_month == "February"] <- "Winter"
HotelData_3$Season[HotelData_3$arrival_date_month == "March" |HotelData_3$arrival_date_month == "April" |HotelData_3$arrival_date_month == "May"] <- "Spring"
HotelData_3$Season[HotelData_3$arrival_date_month == "June" |HotelData_3$arrival_date_month == "July" |HotelData_3$arrival_date_month == "August"] <- "Summer"
HotelData_3$Season[HotelData_3$arrival_date_month == "September" |HotelData_3$arrival_date_month == "October" |HotelData_3$arrival_date_month == "November"] <- "Fall"

HotelData_3$Season <- as.factor(HotelData_3$Season)
skim(HotelData_3)

# Data set removing unused variables in our model
HotelData_Final <- subset(HotelData_3, select = -c(arrival_date_year, arrival_date_month,
                                                   arrival_date_week_number, arrival_date_day_of_month, babies, country,
                                                   assigned_room_type, agent, company, days_in_waiting_list, distribution_channel,
                                                   required_car_parking_spaces,reservation_status,reservation_status_date))

#Confirm season conversion
sum(HotelData_$arrival_date_month == "December")
sum(HotelData_3$arrival_date_month == "January")
sum(HotelData_3$arrival_date_month == "February")
sum(HotelData_Final$Season == "Winter")

install.packages("formattable")
library(formattable)
library(dplyr)
#Reservations by Season
HotelData_Final %>% group_by(Season) %>%
  summarise(cnt = n()) %>%
  mutate(freq = formattable::percent(cnt / sum(cnt))) %>% 
  arrange(desc(freq))

summary(HotelData_Final)
str(HotelData_Final)
library(skimr)
skim(HotelData_Final)

#Exploratory logistic regression model
Logmodel2 <- glm(formula = is_canceled ~ lead_time + Season + stays_in_weekend_nights +
                   stays_in_week_nights + adults + children + is_Domestic + meal + market_segment +
                   distribution_channel + is_repeated_guest + previous_cancellations +
                   previous_bookings_not_canceled + reserved_room_type + booking_changes + 
                   deposit_type + customer_type + adr + total_of_special_requests,
                 data = HotelData_Final,family = "binomial")
summary(Logmodel2)

###Preliminary model on the Reservations data set
str(HotelReservations)
HotelReservations$type_of_meal_plan  <- as.factor(HotelReservations$type_of_meal_plan)
HotelReservations$room_type_reserved <- as.factor(HotelReservations$room_type_reserved)
HotelReservations$arrival_month <- as.factor(HotelReservations$arrival_month)
HotelReservations$market_segment_type <- as.factor(HotelReservations$market_segment_type)
HotelReservations$booking_status <- as.factor(HotelReservations$booking_status)

HotelReservations2 <- HotelReservations[,-2]
HotelReservations3 <- HotelReservations2[,-10]
skim(HotelReservations3)

str(HotelReservations)
# Hotel Reservations logistic regression FOR COMPARISON
Reservations_Model <- glm(formula = booking_status ~ lead_time + arrival_month + no_of_weekend_nights +
                            no_of_week_nights + no_of_adults + no_of_children + type_of_meal_plan + 
                            market_segment_type + repeated_guest + no_of_previous_cancellations +
                            no_of_previous_bookings_not_canceled + room_type_reserved +
                            avg_price_per_room + no_of_special_requests,
                          data = HotelReservations,family = "binomial")
summary(Reservations_Model)

#Split data into City and Resort sets
str(HotelData_Final)
Resort_Data_raw <- HotelData_Final[HotelData_Final$hotel == "Resort Hotel",]
Resort_Data <- Resort_Data_raw[,-2]
City_Data_raw <- HotelData_Final[HotelData_Final$hotel != "Resort Hotel",]
City_Data <- City_Data_raw[,-2]

# Resort logistic regression
Resort_Model <- glm(formula = is_canceled ~ lead_time + Season + stays_in_weekend_nights +
                    stays_in_week_nights + adults + children + meal + market_segment +
                    is_repeated_guest + previous_cancellations + previous_bookings_not_canceled +
                    reserved_room_type + booking_changes + deposit_type + customer_type + adr +
                    total_of_special_requests + is_Domestic,
                  data = Resort_Data, family = "binomial")
summary(Resort_Model)

# City logistic regression
City_Model <- glm(formula = is_canceled ~ lead_time + Season + stays_in_weekend_nights +
                    stays_in_week_nights + adults + children + meal + market_segment +
                    is_repeated_guest + previous_cancellations + previous_bookings_not_canceled +
                    reserved_room_type + booking_changes + deposit_type + customer_type + adr +
                    total_of_special_requests + is_Domestic,
                  data = City_Data, family = "binomial")
summary(City_Model)

# Loading useful correlation analysis packages
analysis_packages <- c('MASS', 'rcompanion', 'lsr', 'vcd', 'DescTools','PerformanceAnalytics')
for (p in analysis_packages) {
  if(!require(p,character.only = TRUE)) {
    install.packages(p, dep = TRUE)  }}
library(rcompanion)
library(MASS)
library(lsr)
library(vcd)
library(DescTools)
library(corrplot)
library(PerformanceAnalytics)
library(dplyr)
head(HotelData_Final)

# Correlating variables for initial analysis 
#Build correlation matrix
df_cor <- HotelData_Final %>% mutate_if(is.character, as.factor)
df_cor <- df_cor %>% mutate_if(is.factor, as.numeric) 
 corr <- cor(df_cor)
 corrprint <- corr
  #prepare to drop duplicates and correlations of 1     
  corr[lower.tri(corr,diag=TRUE)] <- NA 
  #drop perfect correlations
  corr[corr == 1] <- NA 
  #turn into a 3-column table
  corr <- as.data.frame(as.table(corr))
  #remove the NA values from above 
  corr <- na.omit(corr) 
  #sort by highest correlation
  corr <- corr[order(-abs(corr$Freq)),] 

#turn corr back into matrix in order to plot with corrplot
mtx_corr <- reshape2::acast(corr, Var1~Var2, value.var="Freq")

#plot correlations visually
corrplot(mtx_corr, is.corr=FALSE, tl.col="black", na.label=" ", sig.level = 0.5)

#correlation of is_canceled
base.idx <- 1
other.idx <- 2:20
cors <- unname(sapply(other.idx, function(i) cor.test(df_cor[,base.idx], df_cor[,i])$estimate))
pvals <- unname(sapply(other.idx, function(i) cor.test(df_cor[,base.idx], df_cor[,i])$p.value))

print(cors)
print(pvals)

is_canceled <-- corrprint[,"is_canceled", drop=FALSE]
print(is_canceled)

AbsvalCanceled <- abs(is_canceled)
View(AbsvalCanceled)

####CITY
# Correlating City Data variables for initial analysis 
#Build correlation matrix
df_City_cor <- City_Data %>% mutate_if(is.character, as.factor)
df_City_cor <- df_City_cor %>% mutate_if(is.factor, as.numeric) 
City_corr <- cor(df_City_cor)
City_corrprint <- City_corr
#prepare to drop duplicates and correlations of 1     
City_corr[lower.tri(City_corr,diag=TRUE)] <- NA 
#drop perfect correlations
City_corr[City_corr == 1] <- NA 
#turn into a 3-column table
City_corr <- as.data.frame(as.table(City_corr))
#remove the NA values from above 
City_corr <- na.omit(City_corr) 
#sort by highest correlation
City_corr <- City_corr[order(-abs(City_corr$Freq)),] 
#print table
print(City_corr)
#turn corr back into matrix in order to plot with corrplot
mtx_City_corr <- reshape2::acast(City_corr, Var1~Var2, value.var="Freq")

#plot correlations visually
corrplot(mtx_City_corr, is.corr=FALSE, tl.col="black", na.label=" ", sig.level = 0.5)

City_is_canceled <-- City_corrprint[,"is_canceled", drop=FALSE]
print(City_is_canceled)

City_AbsvalCanceled <- abs(City_is_canceled)
View(City_AbsvalCanceled)

####RESORT
# Correlating Resort Data variables above significance for initial analysis 
#Build correlation matrix
df_Resort_cor <- Resort_Data %>% mutate_if(is.character, as.factor)
df_Resort_cor <- df_Resort_cor %>% mutate_if(is.factor, as.numeric) 
Resort_corr <- cor(df_Resort_cor)
Resort_corrprint <- Resort_corr
#prepare to drop duplicates and correlations of 1     
Resort_corr[lower.tri(Resort_corr,diag=TRUE)] <- NA 
#drop perfect correlations
Resort_corr[Resort_corr == 1] <- NA 
#turn into a 3-column table
Resort_corr <- as.data.frame(as.table(Resort_corr))
#remove the NA values from above 
Resort_corr <- na.omit(Resort_corr) 
#sort by highest correlation
Resort_corr <- Resort_corr[order(-abs(Resort_corr$Freq)),] 
#print table
print(Resort_corr)
#turn corr back into matrix in order to plot with corrplot
mtx_Resort_corr <- reshape2::acast(Resort_corr, Var1~Var2, value.var="Freq")

#plot correlations visually
corrplot(mtx_Resort_corr, is.corr=FALSE, tl.col="black", na.label=" ")

Resort_is_canceled <-- Resort_corrprint[,"is_canceled", drop=FALSE]
print(Resort_is_canceled)

Resort_AbsvalCanceled <- abs(Resort_is_canceled)
View(Resort_AbsvalCanceled)

library(ggplot2)
### Correlation EXPLORATION
#Boxplot #1 Cancellations vs. lead_time (use 'is_canceled' as a factor)
ggplot(data=HotelData_Final, aes(x=factor(is_canceled), y=lead_time, )) + 
  geom_boxplot(fill="pink") +
  ggtitle("BoxPlot of City Cancellations by Lead Time") +
  theme(plot.title = element_text(size = 24, face = "bold")) +
  labs(x="Is Canceled = 1", y="Lead time in days") + 
  theme(axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=16), 
        axis.title=element_text(size=22,face="bold"))

#Boxplot #1a City Cancellations vs. lead_time (use 'is_canceled' as a factor)
ggplot(data=City_Data, aes(x=factor(is_canceled), y=lead_time, )) + 
  geom_boxplot(fill="pink") +
  ggtitle("BoxPlot of City Cancellations by Lead Time") +
  theme(plot.title = element_text(size = 24, face = "bold")) +
  labs(x="Is Canceled = 1", y="Lead time in days") + 
  theme(axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=16), 
        axis.title=element_text(size=22,face="bold"))

#Boxplot #1b Resort Cancellations vs. lead_time (use 'is_canceled' as a factor)
ggplot(data=Resort_Data, aes(x=factor(is_canceled), y=lead_time, )) + 
  geom_boxplot(fill="pink") +
  ggtitle("BoxPlot for Resort Cancellations by Lead Time") +
  theme(plot.title = element_text(size = 20, face = "bold")) +
  labs(x="Is Canceled = 1", y="Lead time in days") + 
  theme(axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20), 
        axis.title=element_text(size=20,face="bold"))

#Boxplot #2 Cancellations vs. previous cancellations (use 'is_canceled' as a factor)
ggplot(data=HotelData_3, aes(x=factor(is_canceled), y=previous_cancellations, fill=is_canceled)) + 
  geom_boxplot(fill="pink") +
  ggtitle("Cancellations from Previous Cancellers") +
  theme(plot.title = element_text(size = 20, face = "bold")) +
  labs(x="Is Canceled = 1", y="# of previous cancellations") + 
  theme(axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20), 
        axis.title=element_text(size=20,face="bold"))

#Cancellation Counts
sum(HotelData_Final$is_canceled==1)
#44,115 are canceled out of 118,987

#Visualize percentage of cancelled bookings
CancellationCounts <- table(HotelData_Final$is_canceled)
barplot(prop.table(CancellationCounts)* 100, main="Overall Cancellation Status",
        xlab="Not Cancelled=0   Cancelled=1",
        ylab="Percentage of total bookings",
        legend = rownames(CancellationCounts))

# Cancellations percentage rate by season
HotelData_Final %>% group_by(Season, is_canceled) %>%
  summarise(cnt = n()) %>%
  mutate(freq = formattable::percent(cnt / sum(cnt))) %>% 
  arrange(desc(is_canceled))

# Cancellations percentage rate by season for CITY RESERVATIONS
City_Data %>% group_by(Season, is_canceled) %>%
  summarise(cnt = n()) %>%
  mutate(freq = formattable::percent(cnt / sum(cnt))) %>% 
  arrange(desc(is_canceled))

# Cancellations percentage rate by season for RESORT RESERVATIONS
Resort_Data %>% group_by(Season, is_canceled) %>%
  summarise(cnt = n()) %>%
  mutate(freq = formattable::percent(cnt / sum(cnt))) %>% 
  arrange(desc(is_canceled))

##########################################################
# Cancellations percentage rate by Meal Plan
HotelData_Final %>% group_by(meal, is_canceled) %>%
  summarise(cnt = n()) %>%
  mutate(freq = formattable::percent(cnt / sum(cnt))) %>% 
  arrange(desc(is_canceled))

# Cancellations percentage rate by Market Segment
HotelData_Final %>% group_by(market_segment, is_canceled) %>%
  summarise(cnt = n()) %>%
  mutate(freq = formattable::percent(cnt / sum(cnt))) %>% 
  arrange(desc(is_canceled))

# Cancellations percentage rate by Distribution Channel
HotelData_Final %>% group_by(distribution_channel, is_canceled) %>%
  summarise(cnt = n()) %>%
  mutate(freq = formattable::percent(cnt / sum(cnt))) %>% 
  arrange(desc(is_canceled))

# Cancellations percentage rate by repeat guests
HotelData_Final %>% group_by(is_repeated_guest, is_canceled) %>%
  summarise(cnt = n()) %>%
  mutate(freq = formattable::percent(cnt / sum(cnt))) %>% 
  arrange(desc(is_canceled))

##########################################################
library(magrittr)
# HotelData_Final %>% group_by(Season, is_canceled) %>%
# summarise(cnt = n()) %>%
#  mutate(
#    MaxBySeason = max(is_canceled, na.rm = T),
#    MinBySeason = min(is_canceled, na.rm = T)
#   ) %>%
#  arrange(Season)

#Split into a Canceled data subset and Not Canceled data subset
str(HotelData_Final)
Not_Canceled_Bookings <- HotelData_Final[HotelData_Final$is_canceled==0,]
Canceled_Bookings <- HotelData_Final[HotelData_Final$is_canceled==1,]

str(City_Data)
City_Not_Canceled_Bookings <- City_Data[City_Data$is_canceled==0,]
City_Canceled_Bookings <- City_Data[City_Data$is_canceled==1,]

str(Resort_Data)
Resort_Not_Canceled_Bookings <- Resort_Data[Resort_Data$is_canceled==0,]
Resort_Canceled_Bookings <- Resort_Data[Resort_Data$is_canceled==1,]

# Plots of Total Canceled Bookings by Season
dfSzn<-HotelData_Final[,c('is_canceled','Season')]
tableSzn<-table(dfSzn$is_canceled,dfSzn$Season)
barplot(tableSzn,beside = T, col=c("green","red"),
        main="Cancelation Status by Season",xlab="Season",ylab="# of Cancellations",
        cex.names =1,legend.text =rownames(tableSzn),args.legend = list(x = "topright",title="Canceled=1"))

# Plot of City Canceled Bookings by Season
dfCitySzn<-City_Data[,c('is_canceled','Season')]
tableCitySzn<-table(dfCitySzn$is_canceled,dfCitySzn$Season)
barplot(tableCitySzn,beside = T, col=c("lightblue","darkgreen"),
        main="City Hotel Booking Status by Season",xlab="Season",ylab="# of Cancellations",
        cex.names =1,legend.text =rownames(tableSzn),args.legend = list(x = "topright",title="Canceled=1"))

# Plot of Resort Canceled Bookings by Season
dfRstSzn<-Resort_Data[,c('is_canceled','Season')]
tableRstSzn<-table(dfRstSzn$is_canceled,dfRstSzn$Season)
barplot(tableRstSzn,beside = T, col=c("lightblue","darkgreen"),
        main="Resort Hotel Booking Status by Season",xlab="Season",ylab="# of Cancellations",
        cex.names =1,legend.text =rownames(tableSzn),args.legend = list(x = "topright",title="Canceled=1"))

#Plots to explore cancellation trends among variables
str(HotelData_Final)

# Plots of Canceled Bookings by Meal Plans
dfMP<-HotelData_Final[,c('is_canceled','meal')]
tableMP<-table(dfMP$is_canceled,dfMP$meal)
barplot(tableMP,beside = T, col=c("blue","red"),
        main="Cancelation Status by Meal Plans",xlab="Meal Plans",ylab="# of Cancellations",
        cex.names =1,legend.text =rownames(tableMP),args.legend = list(x = "top",title="Canceled=1"))

MealCount <- table(Canceled_Bookings$meal)
barplot(prop.table(MealCount) * 100, main="Cancelled bookings portioned by Meal Plan",
        xlab="Meal Plans",ylab="Percentage of total cancellations")
MealCount

#Plots of Canceled Bookings by Repeat Guests
dfRPG<-HotelData_Final[,c('is_canceled','is_repeated_guest')]
tableRPG<-table(dfRPG$is_canceled,dfRPG$is_repeated_guest)
barplot(tableRPG,beside = T, col=c("blue","red"),
        main="Cancelation Status by Repeat Guests",xlab="Repeat Guests = 1",ylab="# of Cancellations",
        cex.names =1,legend.text =rownames(tableRPG),args.legend = list(x = "right",title="Canceled=1"))

library(tidyr)
#Cancellations by Deposit Type for Weekend Stays and Mid-Week Stays
dfDp_Wk<-Canceled_Bookings[,c('stays_in_weekend_nights','stays_in_week_nights','deposit_type')]
dfplotDp_Wk<-pivot_longer(dfDp_Wk, -deposit_type, names_to="variable", values_to="value")
ggplot(dfplotDp_Wk,aes(x = deposit_type,y = value)) + 
  geom_bar(aes(fill = variable),stat = "identity",position = "dodge") +
  ggtitle("Cancellations by Deposit Type for Midweek & Weekend Stays") +
  xlab("Cancellations by Deposit Type")+
  ylab("Cancelations Ratio")

#NEW Cancellations by Deposit Type by Season
HotelData_Final %>%
  count(hotel, Season, is_canceled) %>%
  group_by(hotel, Season) %>%
  mutate(proportion = n / sum(n)) %>%
  ggplot(aes(Season, proportion, fill = is_canceled)) + 
  geom_bar(stat="identity", position = "dodge2") +
  ggtitle("Cancellations by Season") +
  xlab("Bookings by Season")+
  ylab("Booking Result Percentages") +
  facet_wrap(~hotel)

#City Cancellations by Deposit Type for Weekend Stays and Mid-Week Stays
dfCity_Dp_Wk<-City_Canceled_Bookings[,c('stays_in_weekend_nights','stays_in_week_nights','deposit_type')]
dfplotCity_Dp_Wk<-pivot_longer(dfCity_Dp_Wk, -deposit_type, names_to="variable", values_to="value")
ggplot(dfplotCity_Dp_Wk,aes(x = deposit_type,y = value)) + 
  geom_bar(aes(fill = variable),stat = "identity",position = "dodge") +
  ggtitle("City Cancellation Ratios by Deposit Type for Midweek & Weekend Stays") +
  xlab("Cancellations by Deposit Type")+
  ylab("Percentage of Total City Cancelations")

#Resort Cancellations by Deposit Type for Weekend Stays and Mid-Week Stays
dfResort_Dp_Wk<-Resort_Canceled_Bookings[,c('stays_in_weekend_nights','stays_in_week_nights','deposit_type')]
dfplotResort_Dp_Wk<-pivot_longer(dfResort_Dp_Wk, -deposit_type, names_to="variable", values_to="value")
ggplot(dfplotResort_Dp_Wk,aes(x = deposit_type,y = value)) + 
  geom_bar(aes(fill = variable),stat = "identity",position = "dodge") +
  ggtitle("Resort Cancelation Ratios by Deposit Type for Midweek & Weekend Stays") +
  xlab("Cancellations by Deposit Type")+
  ylab("Percentage of Total Resort Cancelations")

#Resort Cancellations by Deposit Type for Domestic and Foreign
dfResort_Dp_Wk<-Resort_Canceled_Bookings[,c('is_Domestic','is_Domestic','deposit_type')]
dfplotResort_Dp_Wk<-pivot_longer(dfResort_Dp_Wk, -deposit_type, names_to="variable", values_to="value")
ggplot(dfplotResort_Dp_Wk,aes(x = deposit_type,y = value)) + 
  geom_bar(aes(fill = variable),stat = "identity",position = "dodge") +
  ggtitle("Resort Cancellations by Deposit Type for Midweek & Weekend Stays") +
  xlab("Cancellations by Deposit Type")+
  ylab("Cancellation Ratio")

#Cancelled Rooms by Meal Plan for Weekend Stays and Mid-Week Stays
dfMPStays<-Canceled_Bookings[,c('stays_in_weekend_nights','stays_in_week_nights','meal')]
dfplotMP<-pivot_longer(dfMPStays, -meal, names_to="variable", values_to="value")
ggplot(dfplotMP,aes(x = meal,y = value)) + 
  geom_bar(aes(fill = variable),stat = "identity",position = "dodge") +
  ggtitle("Cancellations by Meal Plan for Midweek & Weekend Stays") +
  xlab("Cancellations by Meal Plan")+
  ylab("Cancellation Ratio")

##################3
#PCA attempt with numerical and categorical variables
install.packages("FactoMineR")
install.packages("vcd")
library(FactoMineR)
library(vcd)
library(factoextra)

# A version of this code worked a couple times then crash my machine... leaving it here fore reference only
#Hotel_famd <- FAMD(HotelData_Final,graph=FALSE)
#Hotel_famd
#Hotel_famd <- FAMD(HotelData_Final,graph=TRUE)

#fviz_famd_ind(Hotel_famd,col.ind = "cos2",
#              gradient.cols = c("blue", "green", "red"),
#              repel = TRUE)

#Hotel_mca <- MCA(HotelData_Final, ncp = 20, graph = FALSE)
#Hotel_mca
#Hotel_mca <- MCA(HotelData_Final, ncp = 20, graph = TRUE)

#Hotel_mca_biplot(HotelData_Final_mca, repel = TRUE, ggtheme = theme_minimal())
