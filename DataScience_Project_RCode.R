# Data Science Project Code R
# Project : Predicting One Family Dwellings Sale Prices

install.packages("tidyverse")
install.packages("magrittr")
install.packages("stringr")


# I. Data import and cleanup ----------

library(readxl)
library(dplyr)

## a. Skipping the first 4 rows which are not relevant headers in the datasets ------
bronx <- read_excel("Les Etudes/Oslo/Projet_DataScience/rollingsales_bronx.xlsx", skip = 4)
brooklyn <- read_excel("Les Etudes/Oslo/Projet_DataScience/rollingsales_brooklyn.xlsx", skip = 4)
manhattan <- read_excel("Les Etudes/Oslo/Projet_DataScience/rollingsales_manhattan.xlsx", skip = 4)
staten_island <- read_excel("Les Etudes/Oslo/Projet_DataScience/rollingsales_statenisland.xlsx", skip = 4)
queens <- read_excel("Les Etudes/Oslo/Projet_DataScience/rollingsales_queens.xlsx", skip = 4)

NYC_property_sales <- bind_rows(bronx, brooklyn, manhattan, staten_island, queens)
head(NYC_property_sales)

## b. Delete individual dataframes to free up memory -------
rm(bronx, brooklyn, manhattan, staten_island, queens)

## c. For clarity when calling a variable, replace borough number with borough name ----------
NYC_property_sales <- NYC_property_sales |>
  mutate(BOROUGH = case_when(
    BOROUGH == 2 ~ "Bronx",
    BOROUGH == 3 ~ "Brooklyn",
    BOROUGH == 1 ~ "Manhattan",
    BOROUGH == 5 ~ "Staten Island",
    BOROUGH == 4 ~ "Queens"))

## d. For clarity when calling a variable, replace column names to lower case and convert CAPITALIZED columns to Title Case-------
library(magrittr)
library(stringr)

NYC_property_sales <- NYC_property_sales |>
  janitor::clean_names() |>
  mutate(neighborhood = str_to_title(neighborhood),
         building_class_category = str_to_title(building_class_category),
         address = str_to_title(address))

## e. Remove duplicates and delete unnecessary columns to avoid biased the results--------
NYC_property_sales <- NYC_property_sales |>
  distinct() |>
  select(-easement)

library(tidyr)
NYC_property_sales <- NYC_property_sales |>
  filter(sale_price >= 10000, gross_square_feet > 0, land_square_feet > 0) |>
  drop_na(c(gross_square_feet, sale_price)) |>
  arrange(borough,neighborhood)


## f. Create a new cleaned file to make data access easier for analysis --------
library(readr)
write_csv(NYC_property_sales, "Les Etudes/Oslo/Projet_DataScience/Cleaned_data_project.csv")



# II. Bivariate relationships exploration-------------

NYC_property_sales <- read_csv("Les Etudes/Oslo/Projet_DataScience/Cleaned_data_project.csv")
glimpse(NYC_property_sales)
sort(table(NYC_property_sales$building_class_at_present))

## a. Study on one single type of building class : one family dwellings ------
# This building class (A.) is the most common building class in the dataframe
NYC_dwellings <- NYC_property_sales |>
  filter(grepl("^A[0-9A-Za-z]", building_class_at_time_of_sale))  
head(NYC_dwellings)

## b. In this scatterplot, we have a first glimpse of the relationship -------
library(ggplot2)
ggplot(NYC_dwellings, aes(x = gross_square_feet, y = sale_price, color = borough)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 20000000)) + 
  xlim(0, 9000) +  
  labs(title = "Relationship Between Sale Price and Gross Square Feet in NYC One Family Dwellings",
       subtitle = "All boroughs combined",
       x = "Size (Gross Square Feet)",
       y = "Sale Price (USD)") +
  theme_minimal()

## c. Plot by borough to have a better insight of the relationship in each ------
ggplot(NYC_dwellings, aes(x = gross_square_feet, y = sale_price)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Relationship Between Sale Price and Gross Square Feet by Borough",
       x = "Size (Gross Square Feet)",
       y = "Sale Price (USD)") +
  facet_wrap(~borough, scales = "free", ncol = 2) + 
  theme_minimal()


# III. Outliers and Data integrity issues ---------------

## a. Copy of the dataframe to not loose any informations ------
NYC_dwellings_original <- NYC_dwellings  

## b. Research of outliers for each suspect borough, to avoid biased results in the analysis --------

outliers_Manhattan <- NYC_dwellings |>
  filter(borough == "Manhattan") |>
  arrange(desc(sale_price))
NYC_dwellings <- NYC_dwellings |>
  filter(!(address == "138-140 West 11 Street" & sale_price >= 40000000))

outliers_Bronx <- NYC_dwellings |>
  filter(borough == "Bronx") |>
  arrange(desc(sale_price))
NYC_dwellings <- NYC_dwellings |>
  filter(!(address == "700 West 247 Street" & sale_price >= 4000000),
         !(address == "4715 Independence Ave" & sale_price >= 4000000))

outliers_Queens <- NYC_dwellings |>
  filter(borough == "Queens") |>
  arrange(desc(sale_price))
NYC_dwellings <- NYC_dwellings |>
  filter(!(address == "38-67 10th Street" & sale_price >= 5000000))

rm(outliers_Bronx, outliers_Manhattan, outliers_Queens)

## c. New scatterplots to have a more precise insight -------
ggplot(NYC_dwellings, aes(x = gross_square_feet, y = sale_price)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Relationship Between Sale Price and Gross Square Feet by Borough",
       x = "Size (Gross Square Feet)",
       y = "Sale Price (USD)") +
  facet_wrap(~borough, scales = "free", ncol = 2) + 
  theme_minimal()


# IV. Linear Regression Models for each Borough - Coefficient Estimates -----------

library(purrr)
library(broom)

NYC_nested <- NYC_dwellings |>
  group_by(borough) |>
  nest()

print(NYC_nested)
print(NYC_nested$data[[1]])

NYC_nested <- NYC_nested |>
  mutate(lin_model = map(data, ~lm(sale_price ~ gross_square_feet, data = .x)))

print(NYC_nested)
summary(NYC_nested$lin_model[[5]])

# Generate a tidy dataframe of coefficient estimates that includes confidence intervals
NYC_nested <- NYC_nested |>
  mutate(tidy_coef = map(lin_model, ~tidy(.x, conf.int = TRUE)))

NYC_nested
print(NYC_nested$tidy_coef[[1]])

# Unnest to a tidy dataframe of coefficient estimates
# Filter to return the slope estimate only
NYC_slope_estimates <- NYC_nested |>
  select(borough, tidy_coef) |>
  unnest(tidy_coef) |>
  filter(term == "gross_square_feet")

NYC_slope_estimates


# V. Linear Regression Model for Boroughs in New York City Combined - Predicions --------

library(tidymodels)
library(xgboost)

## a. Split the data -----
set.seed(1234)
NYC_split <- initial_split(NYC_dwellings_original, prop = 0.8)
NYC_train <- training(NYC_split)
NYC_test <- testing(NYC_split)

## b. Build recipe ------
rec <- recipe(sale_price ~ gross_square_feet, data = NYC_train) |>
  step_dummy(all_factor_predictors())

## c. Train model -----
lm_spec <- linear_reg() |>
  set_engine("lm")

rforest_spec <- rand_forest() |>
  set_engine("ranger") |>
  set_mode("regression")

xgboost_spec <- boost_tree() |>
  set_engine("xgboost") |>
  set_mode("regression")

## d. Set up of a common recipe for all workflows  ------
wf <- workflow() |>
  add_recipe(rec)

# Separate workflows for each model
lm_wf <- wf |>
  add_model(lm_spec)

rforest_wf <- wf |>
  add_model(rforest_spec)

xgboost_wf <- wf |>
  add_model(xgboost_spec)

## e. Fitting each model -----
lm_fit <- fit(lm_wf, data = NYC_train)
rforest_fit <- fit(rforest_wf, data = NYC_train)
xgboost_fit <- fit(xgboost_wf, data = NYC_train)

tidy(lm_fit) |>
  filter(term == "gross_square_feet")

## f. Predict ------
lm_pred <- predict(lm_fit, NYC_test) |>
  rename("pred_lm" = .pred)    

rforest_pred <- predict(rforest_fit, NYC_test) |>
  rename("pred_rforest" = .pred)

xgboost_pred <- predict(xgboost_fit, NYC_test) |>
  rename("pred_xgboost" = .pred)

## g. Combine predictions into a single data frame with actual sale prices and metrics calculation
predictions <- NYC_test |> 
  select(sale_price) |> 
  bind_cols(lm_pred, rforest_pred, xgboost_pred)
head(predictions)

# Calculate RMSE for each model
library(yardstick)
predictions |> 
  metrics(truth = sale_price, estimate = pred_lm) |> 
  filter(.metric == "rmse")

predictions |> 
  metrics(truth = sale_price, estimate = pred_rforest) |> 
  filter(.metric == "rmse")

predictions |> 
  metrics(truth = sale_price, estimate = pred_xgboost) |> 
  filter(.metric == "rmse")