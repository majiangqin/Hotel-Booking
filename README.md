# Hotel-Booking

Team 60's group project GitHub repository for MGT 6203 (Canvas) Spring of 2023 semester.

PROBLEM STATEMENT

Our goal is to model hotel cancellations using two Kaggle datasets and understand the key variables common to cancelled bookings. Our primary dataset is derived from a hotel industry database for two hotels in the country of Portugal: one hotel in the country’s capital, Lisbon, and the other in the southern region of Algarve, a popular beach destination for holiday travelers.

We will apply several analytical methods: logistic regression, K-Nearest Neighbor (KNN), Support Vector Machine (SVM), as well as Holt-Winters forecasting and exponential smoothing to identify booking trends. Both R and Python programming languages will be used.

Data Clean-up Exploratory Analysis.R built by Nuno de Sousa.

DATA OVERVIEW

Both datasets contain similar booking attributes, including Lead-time (days between booking and actual start of reserved stay), number of adults and children, date of stay, market segment, average price, number of special requests. The primary dataset (119,391 data points) contains 3 years of observations (2015-2017) and includes the additional variables of Country and deposit type (refundable/non-refundable); both proved to be very significant. The secondary dataset (36,276 data points) contains 2 years of observations (2017/2018). With these datasets we aim to provide insights into cancellation characteristics.

DATA CLEAN-UP, MUTATIONS, EXPOLRATORY ANALYSIS

After loading the datasets to R, initial inspection revealed the primary dataset required some basic clean-up including:

Removal of null/NA values. Remove datapoints where weekend and weeknight stays both equaled 0. Remove data points where the Adult value equaled 0. Replace meal plan values of 'Undefined' with 'SC' as these represent similar conditions.

 The resulting primary dataset now had 118,338 observations.
One of our original hypotheses was that cancellations were likely to be more prevalent during winter months. As such we created a “Season” variable derived from the date of the booked stay. Additionally, we used the Country value to derive a second new variable of “is Domestic”. The country code of PRT was given a value of 1 and all others 0.

Exploratory analysis revealed several interesting facts and correlations among significant variables.

 •	The city hotel observed 40% cancellation rates with little season variability.
 
 •	The resort hotel observed 30% cancellation rates with a small spike in summer and a small dip in winter.
 
 •	62% of all cancellations were from the domestic clientele, though they only comprised 40% of total bookings.
 
 •	A correlation matrix revealed high correlation between market segment and distribution channel.
 
 •	Approximately 12% of bookings were non-refundable.
    o	An overwhelming 97% of those non-refundable reservations were domestic.
    o	The non-refundable bookings had a lower average cost (90) than no-deposit bookings (104).
    
 •	Required parking spaces is a bit of an anomaly, the overwhelming value (94%) are 0.
    o	Further reading about the reservation process informed this is generally considered a type of special request.
    
 •	A very small number of outliers were observed (adult counts greater than 4), these were not removed as the data was not considered faulty.
Initial correlations of our independent variables to cancellation indicated the following were significant: Deposit type, is Domestic, Lead time, Booking changes, Total special requests, previous cancellations, market segment,

svm_knn_logit_booking.R was built by Jiangqin Ma.

The first part is Data Clean. Converting categorical variables to factors, and removing NA/NULL values in the dataset. Feature selection, using the plot_correlation_funnel() function creates a funnel plot to do the feature selection. After the data was cleaned, this dataset has 118978 observations, which is too large. It takes an exceedingly long time to tune the models, using a sub-sampling ratio of 20% would result in a sub-dataset of approximately 23779 observations. While this can be a reasonable approach for managing large datasets. To evaluate the performance of SVM and KNN models for hotel booking prediction, we split the sub-dataset into training and test sets.

The second part is the SVM model. Tuning the SVM model to find optimal C and sigma, loop over each combination of C and sigma in tune_grid, to find the optimal C and sigma with the lowest misclassification rate. Create an SVM model with optimal C and sigma values. Use confusionMatrix method to get Confusion Matrix and Statistics from the test dataset. Be careful that the hyperparameters tuning part will take a long time, maybe several hours, you may skip this part, just run the part of creating the SVM model with optimal C and sigma values, it will start from line 203, and it will take about half an hour, it depends on your computer's memory.

The third part is the KNN model. Calculate misclassification rates for different k(from 1 to 50). Get the optimal k with the lowest misclassification rate. Create a KNN model with optimal k. Use confusionMatrix method to get Confusion Matrix and Statistics from the test dataset.

The fourth part is the Logistic Regression model. Use the same significant variables and move forward with building a logistic regression model with just them using the same training dataset. Use confusionMatrix method to get Confusion Matrix and Statistics from the test dataset.

Logistic Regression in R (raw code, not .R file) built by Dakota Coomes

Using data from the EDA data files, we convert certain variables to factors, create dummy variables, split the resulting data into training and test datasets, and then run logistic regression models using all predictors and stepwise regression. Then, predictions on these models are made and ROC and AUC are calculated for each model.

Hotel_Cancellations_ logistic regression model.ipnyb (python jupyter notebook file) was built by Gurman Singh

This file analyzes the EDA/generalized dataset to create a initial logictic regression model for predicting hotel cancellations using significant predictors. Utilizing data to create trainging and test datasets we evaluate the accuracy, ROC plot, and AUC.

timeseries.R initial built by Andrea Kirksey

The timeseries.R includes the Holts Winter forecast for 6 months with a frequency of 52. We began by removing the columns with NA values and making a arrival_date column for the resort and hotel. Then, we did some basic EDA. We then deleted columns that didn't let us know the type of hotel booked, or reservation date. Since we're forecasting bookings for both the hotel and resort, we separated the city hotel and resort hotel data. Then, grouped the rows by the arrival date and aggregated that data to a weekly frequency. We did decomposition graphs for both time series to see if we could see any trends or seasonality. Finally making a forecast with a confidence level of 80%-95%. The forecasting begins on line 167.
