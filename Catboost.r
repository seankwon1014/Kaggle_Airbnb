
# package
library(catboost)
library(tidyverse)
library(plyr)

# data load and combine
dt_all = read_csv("analysisData.csv")
scoringData = read_csv("scoringData.csv")

set.seed(1234) ### 555, 1500 are also used for cv manually
dt_all <- dt_all[sample(1:nrow(dt_all)), ]

dt = rbind.fill(dt_all,scoringData)


# selct variables
dt <- select(dt, id, amenities,latitude,	longitude, square_feet, review_scores_cleanliness,	review_scores_checkin,	review_scores_communication,	review_scores_location,	review_scores_value,weekly_price, review_scores_accuracy, price, property_type, room_type, bed_type, cancellation_policy, host_since,	host_response_time,	host_response_rate,	host_is_superhost,	host_listings_count,	host_has_profile_pic,	host_identity_verified,	neighbourhood_cleansed,	is_location_exact,	accommodates,	bathrooms,	bedrooms,	beds,	security_deposit,	cleaning_fee,	guests_included,	extra_people,	minimum_nights,	maximum_nights,	availability_30,	number_of_reviews,	review_scores_rating,	instant_bookable,	is_business_travel_ready,	cancellation_policy,	require_guest_profile_picture,	require_guest_phone_verification,	calculated_host_listings_count,	reviews_per_month,
             last_scraped,
             calendar_last_scraped,
             first_review,
             last_review,
             host_total_listings_count,
             monthly_price,
             availability_60,
             availability_90,
             availability_365,
             host_neighbourhood,
             neighbourhood,
             neighbourhood_group_cleansed,
             city,
             market,
             smart_location,
             calendar_updated
)


# data to unixtime
dt$last_scraped <- as.numeric(as.POSIXct(dt$last_scraped, format="%Y-%m-%d"))
dt$calendar_last_scraped <- as.numeric(as.POSIXct(dt$calendar_last_scraped, format="%Y-%m-%d"))
dt$first_review <- as.numeric(as.POSIXct(dt$first_review, format="%Y-%m-%d"))
dt$last_review <- as.numeric(as.POSIXct(dt$last_review, format="%Y-%m-%d"))


# extract amenity values
am <- c(
  "Kitchen",
  "Heating",
  "Smoke detector",
  "Hangers",
  "Internet",
  "Buzzer/wireless intercom",
  "Washer",
  "Dryer",
  "Hair dryer",
  "Bathtub",
  "Dishes and silverware",
  "Stove",
  "Pets allowed",
  "Dog",
  "Dishwasher",
  "Patio or balcony",
  "Garden or backyard",
  "Cable",
  "Breakfast",
  "Pool",
  "Gym",
  "Wifi",
  "Essentials",
  "Air conditioning",
  "Shampoo",
  "Hair dryer",
  "Laptop friendly workspace",
  "Carbon monoxide detector",
  "Iron",
  "Lock on bedroom door",
  "First aid kit",
  "Refrigerator",
  "Dishes and silverware",
  "Fire extinguisher",
  "Hot water",
  "Oven",
  "Elevator",
  "Coffee maker",
  "Microwave",
  "Family/kid friendly",
  "Bed linens",
  "Step-free access",
  "Self check-in",
  "Private living room",
  "Extra pillows and blankets",
  "Wide doorway",
  "Host greets you",
  "Private entrance",
  "Cable TV",
  "Long term stays allowed",
  "Free parking on premises",
  "Free street parking",
  "Lockbox",
  "Keypad",
  "Luggage dropoff allowed",
  "Ethernet connection",
  "Safety card",
  "Smoking allowed",
  "Well-lit path to entrance",
  "Patio or balcony",
  "Wide clearance to bed",
  "Flat path to front door",
  "Indoor fireplace",
  "Pets live on this property",
  "Wide entryway",
  "Wide hallway clearance",
  "toilet",
  "Accessible-height bed",
  "Accessible-height toilet",
  "BBQ grill",
  "Cleaning before checkout",
  "Hot tub",
  "Paid parking off premises",
  "Pocket wifi",
  "Wide clearance to shower",
  "Crib",
  "Room-darkening shades",
  "Suitable for events",
  "Cat(s)",
  "Front desk/doorperson",
  "High chair",
  "Paid parking on premises",
  "Single level home",
  "Wheelchair accessible",
  "Baby bath",
  "Babysitter recommendations",
  "Changing table",
  "Entire home/apt",
  "EV charger",
  "Handheld shower head",
  "Smart lock",
  "Stair gates"
)

for (i in am) {
  dt[[i]] <- grepl(i, dt$amenities)
  dt[[i]] <- 1*dt[[i]]
}



# numeric cleasning
dt$host_since <- substring(dt$host_since,1,4)
dt$host_response_rate <- as.numeric(gsub("\\%", "", dt$host_response_rate))

# replace t/f
library(data.table)
dt <- data.table(dt) %>%
  .[host_is_superhost == "t", host_is_superhost := "1"] %>%
  .[host_is_superhost == "f", host_is_superhost := "0"] 

dt <- data.table(dt) %>%
  .[host_identity_verified == "t", host_identity_verified := "1"] %>%
  .[host_identity_verified == "f", host_identity_verified := "0"] 

dt <- data.table(dt) %>%
  .[is_location_exact == "t", is_location_exact := "1"] %>%
  .[is_location_exact == "f", is_location_exact := "0"] 

dt <- data.table(dt) %>%
  .[instant_bookable == "t", instant_bookable := "1"] %>%
  .[instant_bookable == "f", instant_bookable := "0"] 

dt <- data.table(dt) %>%
  .[is_business_travel_ready == "t", is_business_travel_ready := "1"] %>%
  .[is_business_travel_ready == "f", is_business_travel_ready := "0"] 

dt <- data.table(dt) %>%
  .[host_has_profile_pic == "t", host_has_profile_pic := "1"] %>%
  .[host_has_profile_pic == "f", host_has_profile_pic := "0"] 

dt <- data.table(dt) %>%
  .[host_identity_verified == "t", host_identity_verified := "1"] %>%
  .[host_identity_verified == "f", host_identity_verified := "0"] 

dt <- data.table(dt) %>%
  .[require_guest_profile_picture == "t", require_guest_profile_picture := "1"] %>%
  .[require_guest_profile_picture == "f", require_guest_profile_picture := "0"] 

dt <- data.table(dt) %>%
  .[require_guest_phone_verification == "t", require_guest_phone_verification := "1"] %>%
  .[require_guest_phone_verification == "f", require_guest_phone_verification := "0"] 



# change variables into numeric
dt <- transform(dt, 
                host_since = as.numeric(host_since),
                host_is_superhost = as.numeric(host_is_superhost),
                host_has_profile_pic = as.numeric(host_has_profile_pic),
                host_identity_verified = as.numeric(host_identity_verified),
                is_location_exact = as.numeric(is_location_exact),
                instant_bookable = as.numeric(instant_bookable),
                is_business_travel_ready = as.numeric(is_business_travel_ready),
                require_guest_profile_picture = as.numeric(require_guest_profile_picture),
                require_guest_phone_verification = as.numeric(require_guest_phone_verification),
                weekly_price = as.numeric(weekly_price),
                monthly_price = as.numeric(monthly_price),
                review_scores_accuracy = as.numeric(review_scores_accuracy)
                
)



# filter numeric variables
dt_numeric <- dt %>% 
  select_if(is.numeric)


# one hot encoding
response_time <- model.matrix(~host_response_time-1,dt)
area <- model.matrix(~neighbourhood_cleansed-1,dt)
cancel <- model.matrix(~cancellation_policy-1,dt)
property <- model.matrix(~property_type-1,dt)
room <- model.matrix(~room_type-1,dt)
bed <- model.matrix(~bed_type-1,dt)

host_neighbourhood <- model.matrix(~host_neighbourhood-1,dt)
neighbourhood <- model.matrix(~neighbourhood-1,dt)
neighbourhood_group_cleansed <- model.matrix(~neighbourhood_group_cleansed-1,dt)
city <- model.matrix(~city-1,dt)
market <- model.matrix(~market-1,dt)
smart_location <- model.matrix(~smart_location-1,dt)
calendar_updated <- model.matrix(~calendar_updated-1,dt)


# data merge
dt_numeric <- cbind(dt_numeric, response_time, area, cancel, property, room, bed,
                    host_neighbourhood,neighbourhood, neighbourhood_group_cleansed,
                    city, market, smart_location, calendar_updated)
dt_matrix <- data.matrix(dt_numeric)
dt_matrix <- as.data.frame(dt_matrix)

# Split based on price
dt <- dt_matrix[complete.cases(dt_matrix$price), ]
sb <- dt_matrix[!complete.cases(dt_matrix$price), ]



# Split price
dt <- dt%>%
  select(-("price"))

sb <- sb %>%
  select(-("price"))

dt_label <- dt_all %>%
  select(price)



# transform to matrix 
dt<- as.matrix(dt)
dt_label <- as.matrix(dt_label)
sb <- as.matrix(sb)

# get the numb 70/30 training test split
num_sam <- round(length(dt_label) * .7)

# training data
train_dt <- dt[1:num_sam,]
train_label <- dt_label[1:num_sam]

# testing data
test_data <- dt[-(1:num_sam),]
test_labels <- dt_label[-(1:num_sam),]

# transform to catboost data
train_pool <- catboost.load_pool(data = train_dt, label = train_label)
test_pool <- catboost.load_pool(data = test_data, label = test_labels)
dsb <- catboost.load_pool(data = sb)
full_pool <- catboost.load_pool(data = dt, label = dt_label)



# parameter
params <- list(iterations=10000,
               depth=13,
               eval_metric='RMSE',
               learning_rate = 0.13,
               l2_leaf_reg = 0.3,
               rsm = 0.4,
               border_count = 255,
               random_seed = 55,
               od_type='Iter',
               bootstrap_type = 'Bernoulli',
               nan_mode = 'Forbidden',
               use_best_model=TRUE)

# modeling
model <- catboost.train(train_pool, test_pool, params)

# modeling with entire data
fmodel <- catboost.train(full_pool, test_pool, params)


# read in scoring data and apply model to generate predictions
preds <- catboost.predict(fmodel, dsb)


submissionFile = data.frame(id = scoringData$id, price = preds)
write.csv(submissionFile, 'C:/Class/FRM1/Kaggle/submission_36_cat_100000.csv',row.names = F)

