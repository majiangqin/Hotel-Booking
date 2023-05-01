rm(list = ls())

library(tidyverse)
library(lubridate)
library(forecast)
bookings <- read.csv("hotel_bookings.csv")
# bookings <- na.omit(bookings)
head(bookings)
# remove columns that have NA and don't seem as important.
bookings %>% 
  select(-is_canceled, -lead_time, -stays_in_weekend_nights, -stays_in_week_nights, -previous_cancellations, 
         -previous_bookings_not_canceled, -assigned_room_type, -booking_changes, -deposit_type, -agent, -company, 
         -days_in_waiting_list, -adr, -required_car_parking_spaces, -total_of_special_requests, -reservation_status, 
         -reservation_status_date, -arrival_date_week_number, -adults, -children, -babies, -market_segment, 
         -is_repeated_guest) %>% 
  # Changing months to numeric value
  mutate(arrival_date_month = case_when(arrival_date_month == "January" ~ 1,
                                        arrival_date_month == "February" ~ 2,
                                        arrival_date_month == "March" ~ 3,
                                        arrival_date_month == "April" ~ 4,
                                        arrival_date_month == "May" ~ 5,
                                        arrival_date_month == "June" ~ 6,
                                        arrival_date_month == "July" ~ 7,
                                        arrival_date_month == "August" ~ 8,
                                        arrival_date_month == "September" ~ 9,
                                        arrival_date_month == "October" ~ 10,
                                        arrival_date_month == "November" ~ 11,
                                        arrival_date_month == "December" ~ 12)) %>% 
  mutate(arrival_date = paste(arrival_date_year, arrival_date_month, arrival_date_day_of_month, sep = "-")) %>% 
  select(-arrival_date_year, -arrival_date_month, -arrival_date_day_of_month) %>% 
  mutate(arrival_date = ymd(arrival_date)) -> bookings
bookings %>% 
  glimpse()

bookings %>% 
  summarize_all(~sum(is.na(.)))

# EDA
# Let's look at the different countries and find the top 10
country <- data.frame(bookings)

# Add a new column called count with a value of 1
country$count <- 1

# Group the "country" column, sum the "count" column
country <- country %>%
  group_by(country) %>%
  summarise(count = sum(count)) %>%
  ungroup()

# Sort by the "count" column 
country <- country %>%
  arrange(desc(count))

# Select the top 10 rows
country <- country %>%
  slice_head(n = 10)

# Create a plot of the "count" column against the "country" column
ggplot(country, aes(x = count, y = country)) +
  geom_bar(stat = "identity") +
  labs(x = "Count", y = "Country")
# Clearly PRT is the country where the highest amount of people come from. Then, GBR and FRA.

# add a new column called "count" with a value of 1
pension <- data.frame(bookings) %>%
  mutate(count = 1)

# Group the data by "meal" column, sum the "count" column, and reset the index
pension <- pension %>%
  group_by(meal) %>%
  summarise(count = sum(count)) %>%
  ungroup()

# Sort the data by the "count" column
pension <- pension %>%
  arrange(desc(count))

# Create a plot of the "count" column against the "meal" column
ggplot(pension, aes(x = count, y = meal)) +
  geom_bar(stat = "identity") +
  labs(x = "Count", y = "Meal")
# According to the plot, most booked BB, then HB, followed by SC.

# Add a new column called "count" with a value of 1
channel <- data.frame(bookings) %>%
  mutate(count = 1)

# Group by the "distribution_channel" column, sum the "count" column
channel <- channel %>%
  group_by(distribution_channel) %>%
  summarise(count = sum(count)) %>%
  ungroup()

# Sort the data by the "count" column 
channel <- channel %>%
  arrange(desc(count))

# Create a plot of the "count" column against the "distribution_channel" column
ggplot(channel, aes(x = count, y = distribution_channel)) +
  geom_bar(stat = "identity") +
  labs(x = "Count", y = "Distribution Channel") 
# According to plot, most bookings are done by TA/TO, Direct, and then, Corporate.

# Add a new column called "count" with a value of 1
customer <- data.frame(bookings) %>%
  mutate(count = 1)

# Groupby the "customer_type" column, sum the "count" column
customer <- customer %>%
  group_by(customer_type) %>%
  summarise(count = sum(count)) %>%
  ungroup()

# Sort the data by the "count" column 
customer <- customer %>%
  arrange(desc(count))

# Create plot of the "count" column against the "customer_type" column
ggplot(customer, aes(x = count, y = customer_type)) +
  geom_bar(stat = "identity") +
  labs(x = "Count", y = "Customer Type") 
# Most are transient

rooms <- bookings %>%
  mutate(count = 1) %>%
  group_by(reserved_room_type) %>%
  summarize(count = sum(count)) %>%
  arrange(desc(count)) %>%
  head(10)

# Plot top room types
ggplot(data = rooms, aes(x = count, y = reserved_room_type)) +
  geom_bar(stat = "identity") +
  labs(x = "Count", y = "Reserved Room Type")
# Most people book room type A, then D, and then E.


# hotel type and reservation date
bookings <- bookings %>% 
  select(-c("meal", "country", "distribution_channel", "reserved_room_type", "customer_type"))

city <- bookings %>% filter(hotel == "City Hotel") %>% select(-hotel) %>% 
  mutate(arrival_date = as.Date(arrival_date)) %>% 
  group_by(arrival_date) %>% summarise(posts_published = n())

resort <- bookings %>% filter(hotel == "Resort Hotel") %>% select(-hotel) %>% 
  mutate(arrival_date = as.Date(arrival_date)) %>% 
  group_by(arrival_date) %>% summarise(posts_published = n())


city
resort

# Aggregate data to weekly frequency, each week's average arrivals
weekly_city<-city %>%
  group_by(week=format(arrival_date,"%Y-%U")) %>%
  summarise(arrival=mean(posts_published))

weekly_city

weekly_resort<-resort %>%
  group_by(week=format(arrival_date,"%Y-%U")) %>%
  summarise(arrival=mean(posts_published))

weekly_resort

weekly_city
city_ts <- ts(weekly_city$arrival, start = c(2015,26), frequency = 52)
city_dec <- decompose(city_ts)

plot(city_dec)


resort_ts <- ts(weekly_resort$arrival,start = c(2015,26), frequency = 52)
resort_dec <- decompose(resort_ts)

plot(resort_dec)

#Holt-Winters exponential smoothing with trend and additive seasonal component.
m2_city <- HoltWinters(city_ts)
m2_city
plot(m2_city )
m2_resort <- HoltWinters(resort_ts)
m2_resort
plot(m2_resort )



#make forecasting for 26 weeks, the confidence level is 80%-95%
m2_for_resort <- forecast(m2_resort, h=26, level=c(80,95))
m2_for_city <- forecast(m2_city, h=26, level=c(80,95))


#visualize our forecasting:
plot(m2_for_resort)
lines(m2_for_resort$fitted, lty=2, col="purple")

plot(m2_for_city)
lines(m2_for_city$fitted, lty=2, col="purple")





# plot(city_dec, xaxt = "n") # xaxt = "n" prevents automatic x-axis labeling
# plot(resort_dec, xaxt = "n") # xaxt = "n" prevents automatic x-axis labeling
# 
# 
# #Holt-Winters exponential smoothing with trend and additive seasonal component.
# m2_city <- HoltWinters(city_ts)
# m2_city
# plot(m2_city )
# m2_resort <- HoltWinters(resort_ts)
# m2_resort
# plot(m2_resort )
# 
# #make forecasting
# m2_for_resort <- forecast(m2_resort, h=50, level=c(80,95))
# m2_for_city <- forecast(m2_city, h=50, level=c(80,95))
# 
# 
# #visualize our forecasting:
# plot(m2_for_resort,xaxt = "n")
# lines(m2_for_resort$fitted, lty=2, col="purple")
# 
# plot(m2_for_city,xaxt = "n")
# lines(m2_for_city$fitted, lty=2, col="purple")
# 
# 
# 
# par(mfrow=c(4,1))
# plot(city_ts, main = "Original")
# plot(city_dec$trend, main = "Trend")
# plot(city_dec$seasonal, main = "Seasonality")
# plot(city_dec$random, main = "Error")
# 
# par(mfrow=c(4,1))
# plot(resort_ts, main = "Original")
# plot(resort_dec$trend, main = "Trend")
# plot(resort_dec$seasonal, main = "Seasonality")
# plot(resort_dec$random, main = "Error")
