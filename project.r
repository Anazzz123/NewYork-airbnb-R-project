#Data importing
data <- read.csv('C:/Newyork airbnb analysis/AB_NYC_2019.csv')
library(dplyr)
library(caret)


#cleaning data AND Transforming it
str(data)

#creating new variables
tidy_airbnb_data <- data%>%
  mutate(total_reviews=number_of_reviews + (reviews_per_month*12))

#renaming the columns
tidy_airbnb_data <- tidy_airbnb_data %>%
  rename(host_listings_count = calculated_host_listings_count,
         availability = availability_365)

#rearranging and dropping incomplete data
tidy_airbnb_data <- tidy_airbnb_data %>%
  select(id, name, host_id, host_name, neighbourhood_group, neighbourhood, latitude, longitude, room_type, price, minimum_nights, number_of_reviews, total_reviews, reviews_per_month, host_listings_count, availability)%>%
  drop_na()

#Exploring the data using summarization
summary(tidy_airbnb_data)

str(tidy_airbnb_data)

table(tidy_airbnb_data$neighbourhood_group)#checkig the frequency of airbnbs in different neighbourhoods

#using factor to see different types of room that are available(Levels)
room <-factor(tidy_airbnb_data$room_type)
print(room)

#Visualizing data using different plots
#1. histogram to show the frequemcy of different price range
hist(tidy_airbnb_data$price,breaks = 50,xlab="PRICE",main = "Histogram of Air bnb price")

#2 Barplot to show the frequency of different room types 
barplot(table(tidy_airbnb_data$room_type),xlab = "ROOM TYPES",ylab="FREQUENCY",main="Frequency of different room types in airbnb" )

#3 Plot to show where different airbnbs are located in different locations(to show the region where most airbnbs are located)
plot(tidy_airbnb_data$longitude, tidy_airbnb_data$latitude, pch = 16, col = 'blue',xlab="Longitude",ylab="Latitude")

#Feature Engineering

#Calculating avg price per neighbourhood group which helps us to understand 
#which neighbourhood has cheaper options
avg_price_neighbourhood_group <- tidy_airbnb_data %>%
  group_by(neighbourhood_group) %>%
  summarise(avg_price = mean(price))


cheapest=min(avg_price_neighbourhood_group$avg_price)

expensive=max(avg_price_neighbourhood_group$avg_price)

#here we find that manhattan has the highest pricing and Bronx has the cheapest


# Create a new feature indicating the popularity level of a listing
tidy_airbnb_data$popularity <- c()


low_popularity_threshold <- 50
medium_popularity_threshold <- 200

for (i in 1:nrow(tidy_airbnb_data)) {
  reviews <- tidy_airbnb_data$number_of_reviews[i]
  
  # Assign the popularity level based on the number of reviews
  if (reviews < low_popularity_threshold) {
    tidy_airbnb_data$popularity[i] <- "Low"
  } else if (reviews < medium_popularity_threshold) {
    tidy_airbnb_data$popularity[i] <- "Medium"
  } else {
    tidy_airbnb_data$popularity[i] <- "High"
  }
}

#calculate distance to the famous landmark central park from every airbnb

central_park_lat <- 40.7829
central_park_long <- -73.9654

tidy_airbnb_data$distance_to_central_park <- c()

tidy_airbnb_data$distance_to_central_park <- geosphere::distHaversine(
  p1 = cbind(tidy_airbnb_data$longitude, tidy_airbnb_data$latitude),
  p2 = cbind(central_park_long, central_park_lat)
)

#now we can find the listing with the least distance to central park
min_distance_row <- tidy_airbnb_data[which.min(tidy_airbnb_data$distance_to_central_park), ]
min_distance_row
#we can also sort the whole listing accordiung to the distance it has to central park if we want

#REGRESSION

# Split the data into a training set and testing set
set.seed(123)
train_indices <- createDataPartition(tidy_airbnb_data$price, p = 0.8, list = FALSE)
train_data <- tidy_airbnb_data[train_indices, ]
test_data <- tidy_airbnb_data[-train_indices, ]

# Perform regression on the training set
reg_model <- lm(price ~ room_type+neighbourhood_group+number_of_reviews+availability, data = train_data)

# Make predictions on the testing set
predictions <- predict(reg_model, newdata = test_data)

# Evaluate the model's performance
rmse <- sqrt(mean((test_data$price - predictions)^2))
r_squared <- cor(test_data$price, predictions)^2

# Print the performance metrics
print(paste("RMSE:", rmse))
print(paste("R-squared:", r_squared))

summary(reg_model)


reg_model <- lm(price ~ . - id - name - host_id - host_name, data = tidy_airbnb_data)

# Print the summary of the model
summary(reg_model)

