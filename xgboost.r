
# package
library(xgboost)
library(tidyverse)
library(plyr)
library(data.table)
library(Metrics)

### data load and combine
dt_all = read_csv("analysisData.csv")
scoringData = read_csv("scoringData.csv")

set.seed(1234)
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
am <- strsplit(as.character(dt$amenities), split="[\\},\"\\{]+")
am <- unlist(am)
am <- am[am != ""]
am <- names(sort(table(am)))

for (i in am) {
  dt[[i]] <- grepl(i, dt$amenities)
  dt[[i]] <- 1*dt[[i]]
}


# numeric cleasning
dt$host_since <- substring(dt$host_since,1,4)
dt$host_response_rate <- as.numeric(gsub("\\%", "", dt$host_response_rate))


# replace t/f
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


# Data merge
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


# transform to xgb.DMatrix 
dtrain <- xgb.DMatrix(data = train_dt, label= train_label)
dtest <- xgb.DMatrix(data = test_data, label= test_labels)
dsb <- xgb.DMatrix(data = sb)
dfull <- xgb.DMatrix(data = dt, label= dt_label)


#value list for grid search
hyper_grid <- expand.grid(
  max.depth = c(5, 7),
  alpha = c(1, 10), 
  min_child_weight = c(4, 6, 8),
  gamma = c(0.01, 0.08, 0.1),
  optimal_trees = 0,               # a place to dump results
  min_RMSE = 0                     # a place to dump results
)

# grid search 
for(i in 1:nrow(hyper_grid)) {
  
  # create parameter list
  params <- list(
    max.depth = hyper_grid$max.depth[i],
    alpha = hyper_grid$alpha[i],
    min_child_weight = hyper_grid$min_child_weight[i],
    gamma = hyper_grid$gamma[i]
  )
  # reproducibility
  set.seed(123)
  
  # train model
  xgb.tune <- xgb.cv(
    params = params,
    data = data.matrix(train_dt),
    label = train_label,
    nround = 300,
    nfold = 3,
    "eval_metric" = "rmse",
    objective = "reg:gamma",  # for regression models
    verbose = 10,               # silent,
    early_stopping_rounds = 30 # stop if no improvement for 10 consecutive trees
  )
  
  # add min training error and trees to grid
  hyper_grid$optimal_trees[i] <- which.min(xgb.tune$evaluation_log$test_rmse_mean)
  hyper_grid$min_RMSE[i] <- min(xgb.tune$evaluation_log$test_rmse_mean)
}

hyper_grid %>%
  dplyr::arrange(min_RMSE) %>%
  head(10)



#CV
xgb.tune <- xgb.cv(
  data = data.matrix(train_dt),
  label = train_label,
  max.depth = 5,
  nround = 300, # max number of boosting iterations
  early_stopping_rounds = 30,
  alpha = 10,
  min_child_weight = 6,
  gamma = 0.08,
  objective   = "reg:gamma",
  nfold = 10,
  eval_metric = "rmse")





# modeling
model <- xgboost(data = dtrain , # the data
                 max.depth = 5,
                 nround = 300, # max number of boosting iterations
                 early_stopping_rounds = 30,
                 alpha = 10,
                 min_child_weight = 6,
                 gamma = 0.08,
                 objective   = "reg:gamma",
                 eval_metric = "rmse")

# read in scoring data and apply model to generate predictions
pred = predict(model,newdata=dtest, na.rm=TRUE)

# model assesment
library(Metrics)
rmse(test_labels, pred)

# feature importance
importance_matrix <- xgb.importance(names(dtrain), model = model)
head(importance_matrix)
write.csv(importance_matrix, 'C:/Class/FRM1/Kaggle/feature.csv',row.names = F)



# modeling with entire data
fmodel <- xgboost(data = dfull , # the data
                  max.depth = 5,
                  nround = 300, # max number of boosting iterations
                  early_stopping_rounds = 30,
                  alpha = 10,
                  min_child_weight = 6,
                  gamma = 0.08,
                  objective   = "reg:gamma",
                  eval_metric = "rmse")




# read in scoring data and apply model to generate predictions
pred = predict(fmodel,newdata=dtest, na.rm=TRUE)

# model assesment
rmse(test_labels, pred)


# pred with sumbit
pred_submit = predict(fmodel,newdata=dsb, na.rm=TRUE)

# construct submision from predictions
submissionFile = data.frame(id = scoringData$id, price = pred_submit)
write.csv(submissionFile, 'C:/Class/FRM1/Kaggle/submission_38_new_xg.csv',row.names = F)

