# Capstone time series analysis
#Ahmed AlRashid
#Date: Dec 09, 2020


# Loading packages 
library(tidyverse)
library(janitor)
library(forecast)
library(zoo)
library(xts)
library(lubridate)
library(tidymodels)
library(modeltime)
library(timetk)   




# Loading data set
inventory <- read_csv("POs LI 20100419 .csv")
inventory <- clean_names(inventory) # clean the names in the data set
inventory <- subset( inventory, select = -x12 ) # To remove unnecessary column 

# Convert currency to a numberic
inventory$unit_price <- as.numeric(gsub('[$,]', '', inventory$unit_price))
inventory$total_li_value <- as.numeric(gsub('[$,]', '', inventory$total_li_value))

glimpse(inventory)
# Converting po_document, po_item and material_no to numaric
inventory$material_no <-  as.numeric(inventory$material_no)
inventory$po_document <-  as.numeric(inventory$po_document)
inventory$po_item <-  as.numeric(inventory$po_item)

sum(is.na(inventory))
glimpse(inventory)

# Changed dates format to Date so we can use it for our time series analysis
# inventory$changed_on <- mdy(inventory$changed_on)
inventory$changed_on <- as.Date(inventory$changed_on,
                                format = "%m/%d/%Y")
typeof(inventory$changed_on)
class(inventory$changed_on)

# cheack for NAs 
sum(is.na(inventory))

# Delete all rows with NA values
inventory <-  na.omit(inventory)

# Adding columns for day, month, year into the data set
inventory$day <- format(as.Date(inventory$changed_on), "%d")
inventory$month <- format(as.Date(inventory$changed_on), "%m")
inventory$year <- format(as.Date(inventory$changed_on), "%Y")

day(inventory$changed_on)
month(inventory$changed_on)
year(inventory$changed_on)
#z <- read.zoo(inventory, FUN = as.yearmon, index.column = inventory$changed_on)

#anyDuplicated(z)
#as.xts(z)
#as.ts(z)


#inventory_ts <- xts(inventory$material_no, inventory$changed_on) 
#class(inventory_ts)
#inventory_ts <- as.ts(inventory_ts)




####################################
#############################

# simplify the data set to a univariate time series with columns, “date” and “value”.
#inventory <- inventory %>% 
  #select(changed_on) %>%
  #mutate(new = paste0(inventory$po_document,inventory$material_no, inventory$po_item))
  #set_names(c("date")) 
  
  #glimpse(inventory)

####################################
#############################
 
#simplify the data set to a univariate time series with columns, “date” and “value”.
inventory <- inventory %>% 
select(changed_on, material_no) %>%

set_names(c("date", "value")) 

glimpse(inventory)
 

  
#xx <- data.frame(x = LETTERS[7:16])
#glimpse(xx) 
#xx <- xx %>%
 #   mutate(y = factor(x))
#glimpse(xx)
#levels((xx$y))
#as.numeric(xx$y)

# sorting the data by date
inventory[order(as.Date(inventory$date, format="%Y-%m-%d")),]

#plot the data
inventory %>%
  plot_time_series(date, value, .interactive = FALSE)

# Spliting the data to train
splits <- inventory %>%
  time_series_split(assess = "3 years", cumulative = TRUE)

# Plotting the split data
splits %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(date, value, .interactive = FALSE)

# fitting a ARIMA model
model_fit_arima <- arima_reg() %>%
  set_engine("auto_arima") %>%
  fit(as.numeric(value) ~ date, training(splits))

model_fit_arima


# Prophet

model_fit_prophet <- prophet_reg(seasonality_yearly = TRUE) %>%
  set_engine("prophet") %>%
  fit(value ~ date, training(splits))

model_fit_prophet


#Preprocessing Recipe

#recipe_spec <- recipe(value ~ date, training(splits)) %>%
  #step_timeseries_signature(date) %>%
  #step_rm(contains("am.pm"), contains("hour"), contains("minute"),
  #        contains("second"), contains("xts")) %>%
  #step_fourier(date, period = 365, K = 5) %>%
 #step_dummy(all_nominal())

#recipe_spec %>% prep() %>% juice()



# Elastic Net

#model_spec_glmnet <- linear_reg(penalty = 0.01, mixture = 0.5) %>%
 # set_engine("glmnet")

#workflow_fit_glmnet <- workflow() %>%
 # add_model(model_spec_glmnet) %>%
  #add_recipe(recipe_spec %>% step_rm(date)) %>%
  #fit(training(splits))


# Random Forest

#model_spec_rf <- rand_forest(trees = 500, min_n = 50) %>%
 # set_engine("randomForest")

#workflow_fit_rf <- workflow() %>%
 # add_model(model_spec_rf) %>%
  # add_recipe(recipe_spec %>% step_rm(date)) %>%
  # fit(training(splits))

#New Hybrid Models

#model_spec_prophet_boost <- prophet_boost(seasonality_yearly = TRUE) %>%
 # set_engine("prophet_xgboost") 

# workflow_fit_prophet_boost <- workflow() %>%
#  add_model(model_spec_prophet_boost) %>%
#  add_recipe(recipe_spec) %>%
#  fit(training(splits))

# workflow_fit_prophet_boost


# Modeltime Table
#model_table <- modeltime_table(
#  model_fit_arima, 
#  model_fit_prophet,
#  workflow_fit_glmnet,
#  workflow_fit_rf,
#  workflow_fit_prophet_boost
#) 

#model_table
