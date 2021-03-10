########################################Part 1: Data Exploration and Cleaning#########################################
#Download Packages
library(lattice)
library(ggplot2)
library(caret)
library(lubridate)
#Read Dataset
data = read.csv("~/Desktop/cu/5200 - R framwork/R/analysisData.csv")
scoringData = read.csv("~/Desktop/cu/5200 - R framwork/R/scoringData.csv")
#Data Exploration
str(data)
# summary(data$room_type)
# summary(data$property_type)
# summary(data$amenities)
# summary(data$instant_bookable)
# summary(data$review_scores_rating)
# summary(data$review_scores_accuracy)
# summary(data$cancellation_policy)
# summary(data$room_type)
# summary(data$property_type)
# summary(data$review_scores_value)
# summary(data$host_identity_verified)
# summary(data$review_scores_communication)
# summary(data$host_since)
# summary(data$host_is_superhost)
# summary(scoringData$host_is_superhost)

#Created a new variable using last_review and first_review.
#(wasn't successful, shows error message when predicting the scoringdata)
data$first_review <- as.Date(data$first_review)
data$last_review <- as.Date(data$last_review)
data$listing_time <- as.numeric(data$last_review - data$first_review)
data$listing_time
#Created a same variable in scoringData
scoringData$first_review <- as.Date(scoringData$first_review)
scoringData$last_review <- as.Date(scoringData$last_review)
scoringData$listing_time <- scoringData$last_review - scoringData$first_review
scoringData$listing_time

#Created a new variable using host_since and today's date. So that we know how long has the host been on the site.
currentdate = Sys.Date()
host_since_new = as.Date(data$host_since)
host_days = as.numeric(currentdate-host_since_new)
#add host_days in a new column
data = cbind(data, host_days)
#replacing the missing values
data$host_days[which(is.na(data$host_days))]=mean(data$host_days,na.rm = TRUE)
#Applying the same to scoringdata
currentdate = Sys.Date()
host_since_new = as.Date(scoringData$host_since)
host_days = as.numeric(currentdate-host_since_new)
#add host_days in a new column
scoringData = cbind(scoringData, host_days)
scoringData$host_days[which(is.na(scoringData$host_days))]=mean(scoringData$host_days,na.rm = TRUE)

#Created new variable in local dataset
loc = grepl(pattern = "TV", data$amenities)
data$has_TV = 0
data$has_TV[loc] = 1

loc2 = grepl(pattern = "Kitchen", data$amenities)
data$has_kitchen = 0
data$has_kitchen[loc2] = 1

loc3 = grepl(pattern = "Wifi", data$amenities)
data$has_wifi = 0
data$has_wifi[loc3] = 1

loc4 = grepl(pattern = "Air conditioning", data$amenities)
data$has_ac = 0
data$has_ac[loc4] = 1

loc5 = grepl(pattern = "Washer", data$amenities)
data$has_washer = 0
data$has_washer[loc5] = 1

loc6 = grepl(pattern = "Gym", data$amenities)
data$has_gym = 0
data$has_gym[loc6] = 1

loc7 = grepl(pattern = "Doorman", data$amenities)
data$has_doorman = 0
data$has_doorman[loc7] = 1

data$amenities
#Created the same set of variables in scoring dataset
loc_scoringdata1 = grepl(pattern = "TV", scoringData$amenities)
scoringData$has_TV = 0
scoringData$has_TV[loc_scoringdata1] = 1

loc_scoringdata2 = grepl(pattern = "Kitchen", scoringData$amenities)
scoringData$has_kitchen = 0
scoringData$has_kitchen[loc_scoringdata2] = 1

loc_scoringdata3 = grepl(pattern = "Wifi", scoringData$amenities)
scoringData$has_wifi = 0
scoringData$has_wifi[loc_scoringdata3] = 1

loc_scoringdata4 = grepl(pattern = "Air Conditioning", scoringData$amenities)
scoringData$has_ac = 0
scoringData$has_ac[loc_scoringdata4] = 1

loc_scoringdata5 = grepl(pattern = "Washer", scoringData$amenities)
scoringData$has_washer = 0
scoringData$has_washer[loc_scoringdata5] = 1

loc_scoringdata6 = grepl(pattern = "Gym", scoringData$amenities)
scoringData$has_gym = 0
scoringData$has_gym[loc_scoringdata6] = 1

loc_scoringdata7 = grepl(pattern = "Doorman", scoringData$amenities)
scoringData$has_doorman = 0
scoringData$has_doorman[loc_scoringdata7] = 1

#check missing value
sum(is.na(data$has_kitchen)) #0
sum(is.na(data$has_TV))      #0
##listing_time has NA values
sum(is.na(data$listing_time))
data$listing_time[which(is.na(data$listing_time))]=mean(data$listing_time,na.rm = TRUE)
sum(is.na(scoringData$listing_time))
sum(is.na(data$listing_time))  #now listing_time is clean
#missing values in beds 
summary(data$beds)
str(data$beds)
str(scoringData$beds)
sum(is.na(data$beds))
sum(is.na(scoringData$beds))
data$beds[which(is.na(data$beds))]=mean(data$beds,na.rm = TRUE)
sum(is.na(data$beds))
scoringData$beds[which(is.na(scoringData$beds))]=mean(scoringData$beds,na.rm = TRUE)
sum(is.na(scoringData$beds))
#checking missing values, all are zero.
summary(data$guests_included)
summary(data$availability_90)
summary(data$extra_people)
summary(data$cancellation_policy)

sum(is.na(data$guests_included))
sum(is.na(data$availability_90))
sum(is.na(data$extra_people))
sum(is.na(data$cancellation_policy))
sum(is.na(data$availability_60))
sum(is.na(data$availability_30))

sum(is.na(scoringData$calculated_host_listings_count_entire_homes))   #0
sum(is.na(data$calculated_host_listings_count_entire_homes))
sum(is.na(data$calculated_host_listings_count))
sum(is.na(scoringData$calculated_host_listings_count))
sum(is.na(scoringData$calculated_host_listings_count_shared_rooms))
sum(is.na(data$calculated_host_listings_count_shared_rooms))


########################################Part 2: Modeling (Random Forest)############################################
#split traning and testing dataset
set.seed(1029)
split = sample(1:nrow(data), nrow(data)*0.70)
train = data[split,]
test = data[-split,]
nrow(train)
nrow(test)

library(randomForest)
set.seed(1029)
trControl = trainControl(method = "cv", number = 3)
tuneGrid = expand.grid(mtry = 10)
str(data)

sum(is.na(data$beds))
sum(is.na(scoringData$beds))
summary(scoringData$room_type)
summary(test$room_type)
summary(scoringData$bed_type)
summary(train$bed_type)
summary(train$ host_identity_verified)
summary(scoringData$host_identity_verified)
summary(scoringData$room_type)
summary(train$room_type)
###needs to drop property_type because the scoring data doesn't have the same factors as the training dataset
summary(scoringData$property_type)
summary(train$property_type)
summary(scoringData$instant_bookable)
summary(train$instant_bookable)
##needs to drop cancellation policy
summary(scoringData$cancellation_policy)
summary(train$cancellation_policy)

#construct random forest model (This is the model that yields the best result, that is the lowest RMSE score.)
cvforest = train(price~ 
                   minimum_nights+ review_scores_rating+bathrooms+bedrooms+
                   number_of_reviews+host_identity_verified+instant_bookable+
                   review_scores_accuracy+review_scores_cleanliness+
                   review_scores_checkin+review_scores_communication+
                   review_scores_location+review_scores_value+cancellation_policy+
                   room_type+bed_type+minimum_nights_avg_ntm+
                   neighbourhood_group_cleansed+ #listing_time+
                   has_TV+has_kitchen+has_wifi+has_ac+guests_included+availability_90+
                   availability_30+availability_60+extra_people+
                   calculated_host_listings_count_entire_homes+
                   calculated_host_listings_count+calculated_host_listings_count_private_rooms+
                   calculated_host_listings_count_shared_rooms+has_gym+has_doorman+has_washer+beds+host_days,
                data=train,
                 ntree = 100,
                 trControl = trControl,
                 tuneGrid = tuneGrid)

pred_cvforest = predict(cvforest, newdata=test)
rmse_cvForest = sqrt(mean((pred_cvforest-test$price)^2)); rmse_cvForest

# Read scoring data and apply model to generate predictions
# scoringData = read.csv('~/Desktop/cu/5200 - R framwork/R/pricelala2/scoringData.csv')
pred = predict(cvforest,newdata=scoringData)
# Construct submission from predictions
submissionFile = data.frame(id = scoringData$id, price = pred)
write.csv(submissionFile, "~/Desktop/cu/5200 - R framwork/R/submission1002.csv",row.names = F)
#RMSE = 63.01873
