# loading the required packages
library(tidyverse)
library(palmerpenguins)
library(mice)
library(randomForest)

# including/defining the the needed modules
source("functions/split_to_train_test.R")
count_NAs <- function(df) df |>  summarise(across(everything(), \(x) sum(is.na(x))))

# loading the data set
dataset <- penguins

####################### Data Exploration/Preparation #######################
## Review the data set's structure ----------------------------------------
str(dataset)
dataset |> count_NAs() # to check count of NAs in each variable


## Impute missing data -----------------------------------------------------
imputed_data <- mice(dataset, m = 5, method = "rf")
imputed_data <- complete(imputed_data, 1) # 1 represents here the first prediction out of five

imputed_data |> count_NAs() # to double check the count of NAs


## Divide the data set for train and test purposes -------------------------
set.seed(123)

splitted_dfs <- split_to_train_test(imputed_data, train_prop = 0.8)
train_data <- splitted_dfs$train_data
test_data <- splitted_dfs$test_data


############################ Training the Model ############################ 
set.seed(123) # for reproducibility purposes
palmer_penguins_model <- randomForest(species ~ island + bill_length_mm + bill_depth_mm + flipper_length_mm + body_mass_g + sex,
                                      data = train_data,     # Data set
                                      ntree = 1000,          # Number of trees to grow
                                      mtry = 2)              # Number of branch variables

summary(palmer_penguins_model) # to view the summary of the model


############################ Testing the Model ############################
# rename the species column to later cross check with the prediction
test_data <- test_data |> rename(species.original = species)

# predicting the species using the given features in test data
predictions <- predict(palmer_penguins_model,   # trained model
                      newdata = test_data,      # test data set
                      type = "class")

test_data$species.predicted <- predictions

# exporting the prediction result
write.csv(test_data, "output/palmer_pinguins_predicted_data.csv")
