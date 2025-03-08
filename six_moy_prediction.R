############# Loading Libraries Function ####################

load_libraries <- function(packages) {
  # Check for missing packages
  missing_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  
  # install missing packages
  if(length(missing_packages) > 0) {
    install.packages(missing_packages)
  }
  
  # Load all packages
  lapply(packages, library, character.only = TRUE)
  
  cat("All packages are loaded succesfully.\n")
}


# Loading necessary libraries
load_libraries(c("tidyverse", "lubridate", "stats", "ggplot2", "corrplot", "stringr", "stringi",
                 "tidymodels", "modeldata", "themis", "vip", "baguette", "janitor", "rvest",
                 "yardstick", "gsheet", "caret", "randomForest", "here", "tibble", "dplyr", "ISAR", "tidyr", "mgcv",
                 "teamcolors", "baseballr", "Lahman", "remotes", "ggcorrplot", "broom", "readr", "glmnet", "xgboost", "Matrix", "Metrics"))

# Load only the necessary functions from 'car'
library(car, exclude = "select")

# Turning off warning messages
options(warnings = 0)

######################## Loading Data ##############################

# Loading 6MOY data
six_moy_df <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1SJqNE8qLEsoNd-S583kjZp_qoUm3oqjlZE3yypNzk68/edit?gid=0#gid=0")

# The following data is from 1947 to 2025 found through Kaggle
advanced_df <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1t40BXyuciFV5tM-O8SvhtNMBzJ-cczoPLEQcLUm-858/edit?gid=1880518936#gid=1880518936")

per_100_df <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1AQTsO3lp8V1UNgnR2aWfbDwxZhsmrEOIT5nr_LqWrqQ/edit?gid=1300008067#gid=1300008067")

player_per_game_df <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1UrdY7BQL74ZonAOIaYTKdk310W_C-OnPKaN0DnhnsM8/edit?gid=1967354614#gid=1967354614")


# We are going to only use data from 2004 and forward since our 6MOY data is from 2004 through 2024. Season 2025 will be used for prediction
advanced_df <- advanced_df %>% 
  filter(season >= 2004)

per_100_df <- per_100_df %>% 
  filter(season >= 2004)

player_per_game_df <- player_per_game_df %>% 
  filter(season >= 2004)


################### Cleaning Datasets ##########################


### Joining the datasets that include the data from 2004 to 2025 ###
player_data <- player_per_game_df %>% 
  left_join(advanced_df, by = c("seas_id", "season", "player_id", "player", "birth_year", "pos", "age", "experience", "lg", "tm", "g"))

player_data <- player_data %>% 
  left_join(per_100_df, by = c("seas_id", "season", "player_id", "player", "birth_year", "pos", "age", "experience", "lg", "tm", "g",
                               "gs", "fg_percent", "ft_percent", "x3p_percent", "x2p_percent", "mp"))

# Looking at structure
str(player_data)

# Removing unecessary variables
player_data <- player_data %>% 
  select(-c(seas_id, player_id, birth_year, pos, lg))

# Checking NAs
colSums(is.na(player_data))

# Filling in NAs with 0. The players with the missing data are likely to not have attempted shots or have low playing time
player_data <- player_data %>%
  mutate(across(everything(), ~replace_na(.x, 0)))

# Checking NAs again
colSums(is.na(player_data))


### Creating dataset for potential 6MOY in 2025 ###
six_moy_2025 <- player_data %>% 
  filter(season == 2025, # selecting data for this season
         g > gs,  # selecting players who have played more games than they have started
         g > 40)  # getting rid of players who have played a low number of games
# In order to qualify for an end of season award, players must have played a minimum of 65 games.
# At this point of the season, most teams have played around 60 games. 
# So players must have at least played about 40 games. (It's probably more but we'll go with 40 games)


### Creating dataset for previous 6moy ###

# Looking at rows 172, 311, 312, and 329 through 342, they were not players so we are removing them
six_moy_df <- six_moy_df %>%
  filter(!row_number() %in% c(172, 311, 312, 329:342))

# Looking at structure
str(six_moy_df)

# Changing variables to be numeric
six_moy_df <- six_moy_df %>% 
  mutate(across(-c(Rank, Player, Tm, id), as.numeric))

# Renaming Season as season to make it easier to join with other datasets
six_moy_df <- six_moy_df %>% 
  rename(season = Season)

# Renaming some variables to make joining easier and removing some variables that we already have data for
six_moy_df_cleaned <- six_moy_df %>% 
  rename(
    player = Player,
    age = Age,
    tm = Tm
  ) %>% 
  select(-c(G, MP, PTS, TRB, AST, STL, BLK, `FG%`, `3P%`, `FT%`, WS, `WS/48`, id))

# Joining 6moy data with player data
df <- six_moy_df_cleaned %>% 
  left_join(player_data, by = c("player", "age", "tm", "season"))

# Checking for NAs
colSums(is.na(df))

# Removing NAs. There are two NAs and they are both for JJ Reddick for 2011 and 2013.
# He was last in voting for both years so it wont make a difference if we remove him.
df <- df %>%
  filter(!row_number() %in% c(171, 207))


############# Exploratory Data Analysis ##################


# Select only numeric columns and compute correlation
cor_matrix <- cor(select_if(df, is.numeric), use = "pairwise.complete.obs")

# Extract correlations with K%
share_cor <- cor_matrix[, "Share"] %>%
  sort(decreasing = TRUE) %>%
  as.data.frame() %>%
  rename(Correlation = ".")

# View top correlations
print(share_cor)

# Create correlation matrix heatmap
ggcorrplot(share_cor, 
           method = "square",   # Use "square" or "circle" instead of "color"
           type = "lower", 
           lab = FALSE, 
           lab_size = 3, 
           colors = c("blue", "white", "red"), 
           title = "Correlation Matrix") +
  theme(axis.text.x = element_text(size = 8, angle = 90, hjust = 1),
        axis.text.y = element_text(size = 8))

# Points per game vs Share scatterplot
ggplot(data = df, 
       aes(x = pts_per_game, y = Share, color = (Rank == 1))) +
  geom_point(alpha = 0.8) +
  scale_color_manual(name = "6MOY",
                     values = c("FALSE" = "red", "TRUE" = "blue"), 
                     labels = c("FALSE" = "Lost", "TRUE" = "Won")) +
  labs(title = "Points per Game vs. 6MOY Share") +
  theme_bw()

# Points per game vs Share histogram
ggplot(data = df, aes(x = pts_per_game, fill = (Rank == 1))) +
  geom_histogram(binwidth = 2, alpha = 0.5, position = "identity") +
  scale_fill_manual(name = "6MOY",
                    values = c("FALSE" = "red", "TRUE" = "blue"), 
                    labels = c("FALSE" = "Lost", "TRUE" = "Won")) +
  labs(title = "Distribution of Points per Game by 6MOY Status",
       x = "Minutes per Game",
       y = "Count") +
  theme_bw()


# Field Goal Attempts per game vs Share
ggplot(data = df, 
       aes(x = fga_per_game, y = Share, color = (Rank == 1))) +
  geom_point(alpha = 0.8) +
  scale_color_manual(name = "6MOY",
                     values = c("FALSE" = "red", "TRUE" = "blue"), 
                     labels = c("FALSE" = "Lost", "TRUE" = "Won")) +
  labs(title = "Field Goal Attempts per Game vs. 6MOY Share") +
  theme_bw()


# VORP vs Share
ggplot(data = df, 
       aes(x = vorp, y = Share, color = (Rank == 1))) +
  geom_point(alpha = 0.8) +
  scale_color_manual(name = "6MOY",
                     values = c("FALSE" = "red", "TRUE" = "blue"), 
                     labels = c("FALSE" = "Lost", "TRUE" = "Won")) +
  labs(title = "Value Over Replacement Player vs. 6MOY Share") +
  theme_bw()

# WS vs Share
ggplot(data = df, 
       aes(x = ws, y = Share, color = (Rank == 1))) +
  geom_point(alpha = 0.8) +
  scale_color_manual(name = "6MOY",
                     values = c("FALSE" = "red", "TRUE" = "blue"), 
                     labels = c("FALSE" = "Lost", "TRUE" = "Won")) +
  labs(title = "Win Share vs. 6MOY Share") +
  theme_bw()

# OBPM vs Share
ggplot(data = df, 
       aes(x = obpm, y = Share, color = (Rank == 1))) +
  geom_point(alpha = 0.8) +
  scale_color_manual(name = "6MOY",
                     values = c("FALSE" = "red", "TRUE" = "blue"), 
                     labels = c("FALSE" = "Lost", "TRUE" = "Won")) +
  labs(title = "Offensive Box Plus/Minus vs. 6MOY Share") +
  theme_bw()

# Minutes per game vs Share
ggplot(data = df, 
       aes(x = mp_per_game, y = Share, color = (Rank == 1))) +
  geom_point(alpha = 0.8) +
  scale_color_manual(name = "6MOY",
                     values = c("FALSE" = "red", "TRUE" = "blue"), 
                     labels = c("FALSE" = "Lost", "TRUE" = "Won")) +
  labs(title = "Minutes per Game vs. 6MOY Share") +
  theme_bw()



########################### Linear Regression Model ################################

# We are going to start with a linear model with the following set of predictors
# We will test one at a time and check their accuracy

# Features for prediction
features <- c("pts_per_game", "fga_per_game", "vorp", "ws") # 57.1% Accuracy, r2 = 0.399

features <- c("pts_per_game", "fga_per_game", "vorp", "ws", "obpm", "mp_per_game") # 57.1% Accuracy, r2 = 0.407


target <- "Share"

# Setting seed
set.seed(123)

# Initializing counters
correct_predictions <- 0
total_years <- length(unique(df$season))
mae_values <- c()
r2_values <- c()

# Loop through each year for leave one year out cross-validation
for (yr in unique(df$season)) {
  
  # Split training and test data
  train_data <- df %>% filter(season != yr)
  test_data <- df %>% filter(season == yr)
  
  # Train Linear Regression model
  formula <- as.formula(paste(target, "~", paste(features, collapse = " + ")))
  model <- lm(formula, data = train_data)
  
  # Predict Share for test set
  test_data$Predicted_Share <- predict(model, test_data)
  
  # Compute MAE and R-Squared for this year
  y_test <- test_data$Share  # Actual Share values
  mae_values <- c(mae_values, mae(actual = y_test, predicted = test_data$Predicted_Share))
  r2_values <- c(r2_values, cor(y_test, test_data$Predicted_Share)^2)
  
  # Get the player with the highest predicted Share
  predicted_six_moy <- test_data %>%
    filter(Predicted_Share == max(Predicted_Share, na.rm = TRUE)) %>%
    pull(player) %>%
    first()
  
  actual_six_moy <- test_data %>%
    filter(Rank == "1") %>%
    pull(player) %>%
    first()
  
  # Check if both variables have values before comparing
  if (!is.na(predicted_six_moy) && !is.na(actual_six_moy) && predicted_six_moy == actual_six_moy) {
    correct_predictions <- correct_predictions + 1
  }
}


# Compute final metrics
lm_average_mae <- mean(mae_values, na.rm = TRUE)
lm_average_r2 <- mean(r2_values, na.rm = TRUE)
lm_accuracy <- correct_predictions / total_years

# Print results
print(lm_average_mae)
print(lm_average_r2)
print(lm_accuracy)

# The accuracy for both set of features were not great, with both achieving an accuracy of 57.1%.
# We will continue to explore other models.


########################## Random Forest Model ############################

# We are now going to try a random forest model.
# Same idea, test one set of features at a time


# Features for prediction
features <- c("pts_per_game", "fga_per_game", "vorp", "ws") # 66.7% Accuracy, r2 = 0.508

features <- c("pts_per_game", "fga_per_game", "vorp", "ws", "obpm", "mp_per_game") # 76.2% Accuracy, r2 = 0.491

# Initialize counters
correct_predictions <- 0
total_years <- length(unique(df$season))
mae_values <- c()
r2_values <- c()

# Loop through each year for Leave-One-Year-Out Cross-Validation
for (yr in unique(df$season)) {
  
  # Split training and test data
  train_data <- df %>% filter(season != yr)
  test_data <- df %>% filter(season == yr)
  
  # Convert features into a formula format for Random Forest
  formula <- as.formula(paste("Share", "~", paste(features, collapse = " + ")))
  
  # Train Random Forest model
  rf_model <- randomForest(formula, data = train_data, ntree = 500, importance = TRUE)
  
  # Predict Share for test set
  test_data$Predicted_Share <- predict(rf_model, test_data)
  
  # Compute MAE and R-Squared for this year
  y_test <- test_data$Share  # Actual Share values
  mae_values <- c(mae_values, mae(actual = y_test, predicted = test_data$Predicted_Share))
  r2_values <- c(r2_values, cor(y_test, test_data$Predicted_Share)^2)
  
  # Get the player with the highest predicted Share
  predicted_six_moy <- test_data %>% filter(Predicted_Share == max(Predicted_Share)) %>% pull(player)
  actual_six_moy <- test_data %>% filter(Rank == "1") %>% pull(player)
  
  # Check if prediction is correct
  if (predicted_six_moy == actual_six_moy) {
    correct_predictions <- correct_predictions + 1
  }
}

# Compute final metrics
rf_accuracy <- correct_predictions / total_years
rf_average_mae <- mean(mae_values, na.rm = TRUE)
rf_average_r2 <- mean(r2_values, na.rm = TRUE)

# Print results
print(rf_average_mae)
print(rf_average_r2)
print(rf_accuracy)

# Random forest seems to perform much better with an accuracy of 76.2% and a r-squared of 0.491.


###################### Gradient Boosting Model (XGBoost) ####################

# We are now going to explore a XGBoost model since the random forest model performed relatively well.
# A XGBoost model would be ideal because it's more powerful than random forest because it boosts weak decision trees,
# reducing errors and improving predictive accuracy.

# Once again, we are going to run this model twice with both set of features.


# Features for prediction
features <- c("pts_per_game", "fga_per_game", "vorp", "ws") # 66.7% Accuracy, r2 = 0.479

features <- c("pts_per_game", "fga_per_game", "vorp", "ws", "obpm", "mp_per_game") # 71.4% Accuracy, r2 = 0.464

set.seed(123)

# Initialize counters
correct_predictions <- 0
total_years <- length(unique(df$season))
mae_values <- c()
r2_values <- c()

# Loop through each year for Leave-One-Year-Out Cross-Validation
for (yr in unique(df$season)) {
  
  # Split training and test data
  train_data <- df %>% filter(season != yr)
  test_data <- df %>% filter(season == yr)
  
  # Convert data to matrix format for XGBoost
  X_train <- as.matrix(train_data %>% select(all_of(features)))
  y_train <- train_data$Share
  
  X_test <- as.matrix(test_data %>% select(all_of(features)))
  y_test <- test_data$Share   # Actual Share values
  
  # Convert to XGBoost DMatrix format
  dtrain <- xgb.DMatrix(data = X_train, label = y_train)
  dtest <- xgb.DMatrix(data = X_test)
  
  # Train XGBoost model
  xgb_model <- xgboost(data = dtrain, 
                       nrounds = 100, 
                       objective = "reg:squarederror",
                       max_depth = 6, 
                       eta = 0.1, 
                       subsample = 1, 
                       colsample_bytree = 1,
                       verbose = 0)
  
  # Predict Share for test set
  test_data$Predicted_Share <- predict(xgb_model, dtest)
  
  # Compute MAE and R-Squared for this year
  mae_values <- c(mae_values, mae(y_test, test_data$Predicted_Share))
  r2_values <- c(r2_values, cor(y_test, test_data$Predicted_Share)^2)
  
  # Get the player with the highest predicted Share
  predicted_six_moy <- test_data %>% filter(Predicted_Share == max(Predicted_Share)) %>% pull(player)
  actual_six_moy <- test_data %>% filter(Rank == "1") %>% pull(player)
  
  # Check if prediction is correct
  if (predicted_six_moy == actual_six_moy) {
    correct_predictions <- correct_predictions + 1
  }
}

# Compute final metrics
xgb_average_mae <- mean(mae_values)
xgb_average_r2 <- mean(r2_values)
xgb_accuracy_xgb <- correct_predictions / total_years

# Printing results
print(xgb_average_mae)
print(xgb_average_r2)
print(xgb_accuracy_xgb)

# The second set of features is still showing that they are the better predictors.
# The second set of features achieved 71.4% Accuracy and r2 = 0.464.
# This is on par with the random forest model but it has a lower r2, so for that reason, random forest is still the better model.



################## Trying Unsupervised Models / PCA ##########################


# Drop excluded variables
excluded_vars <- c("Rank", "age", "First", "Pts Won", "Pts Max", "Share", "season")  # Target and voting data excluded and season
pca_data <- df %>%
  select(-one_of(excluded_vars)) %>%
  select_if(is.numeric)  # Keep only numeric columns

# Standardize the data
pca_data_scaled <- scale(pca_data)

# Perform PCA
pca_model <- prcomp(pca_data_scaled, center = TRUE, scale. = TRUE)

# Variance explained by each principal component
explained_variance <- data.frame(
  Principal_Component = 1:length(pca_model$sdev),
  Cumulative_Variance = cumsum((pca_model$sdev)^2 / sum((pca_model$sdev)^2))
)

# Plot the explained variance
ggplot(explained_variance, aes(x = Principal_Component, y = Cumulative_Variance)) +
  geom_line() + geom_point() +
  labs(title = "Cumulative Variance Explained by Principal Components",
       x = "Number of Principal Components",
       y = "Cumulative Explained Variance") +
  theme_minimal()

# Print explained variance table
print(explained_variance)


# Convert PCA loadings to a data frame
pca_loadings <- as.data.frame(pca_model$rotation)

# Convert to long format for plotting
loadings_long <- pca_loadings %>%
  rownames_to_column(var = "Variable") %>%
  pivot_longer(cols = starts_with("PC"), names_to = "Principal_Component", values_to = "Loading")

# Keep only the first 10 principal components
loadings_long <- loadings_long %>%
  filter(Principal_Component %in% paste0("PC", 1:10)) %>%
  mutate(Absolute_Loading = abs(Loading))

# Ensure the PCs are ordered numerically
loadings_long$Principal_Component <- factor(loadings_long$Principal_Component, 
                                            levels = paste0("PC", 1:10))  # Ordered from PC1 to PC10

# Select top 7 contributing variables for each of the first 10 PCs
top_n <- 7  # Number of top variables per PC
top_loadings <- loadings_long %>%
  group_by(Principal_Component) %>%
  slice_max(order_by = Absolute_Loading, n = top_n)

# Plot the contributions with ordered PCs
ggplot(top_loadings, aes(x = reorder(Variable, Absolute_Loading), y = Absolute_Loading, fill = Principal_Component)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +  # Flip axes for better readability
  facet_wrap(~ Principal_Component, scales = "free_y", ncol = 2) +  # Arrange in multiple columns
  labs(title = "Top Contributing Variables to First 10 Principal Components",
       x = "Variables",
       y = "Absolute Loading Strength") +
  theme_minimal()




# Prepare PCA-transformed dataset
pca_transformed <- as.data.frame(pca_model$x[, 1:14])  # Select first 14 PCs
pca_transformed$Share <- df$Share  # Target variable
pca_transformed$season <- df$season  # Season variable
pca_transformed$player <- df$player  # Player names (for accuracy check)

# Initialize counters for evaluation metrics
correct_predictions <- 0
total_years <- length(unique(pca_transformed$season))
mae_values <- c()
r2_values <- c()

# Loop through each year for Leave-One-Year-Out Cross-Validation
for (yr in unique(pca_transformed$season)) {
  
  # Split training and test data
  train_data <- pca_transformed %>% filter(season != yr)
  test_data <- pca_transformed %>% filter(season == yr)
  
  # Train Linear Regression model using the first 14 PCs
  model <- lm(Share ~ ., data = train_data %>% select(-season, -player))  # Exclude non-numeric variables
  
  # Predict 6MOY Share for test set
  test_data$Predicted_Share <- predict(model, test_data)
  
  # Compute MAE and R-Squared for this year
  y_test <- test_data$Share  # Actual 6MOY Share values
  mae_values <- c(mae_values, mean(abs(y_test - test_data$Predicted_Share), na.rm = TRUE))
  r2_values <- c(r2_values, cor(y_test, test_data$Predicted_Share, use = "complete.obs")^2)
  
  # Get the player with the highest predicted 6MOY Share
  predicted_six_moy <- test_data %>% filter(Predicted_Share == max(Predicted_Share)) %>% pull(player)
  actual_six_moy <- test_data %>% filter(Share == max(Share)) %>% pull(player)  # Actual 6MOY winner
  
  # Check if prediction is correct
  if (length(predicted_six_moy) > 0 && length(actual_six_moy) > 0 && predicted_six_moy == actual_six_moy) {
    correct_predictions <- correct_predictions + 1
  }
}

# Compute final evaluation metrics
average_mae <- mean(mae_values, na.rm = TRUE)
average_r2 <- mean(r2_values, na.rm = TRUE)
accuracy <- correct_predictions / total_years  # Accuracy based on correct predictions

# Print results
print(average_mae)
print(average_r2)
print(accuracy)

# PCA achieved an accuracy of 71.4% but with a low r-squared of 0.393. 
# Random forest is still the better model.



########### 2025 6MOY Prediction #################


# Select only the features used in the final model
features <- c("pts_per_game", "fga_per_game", "vorp", "ws", "obpm", "mp_per_game")

set.seed(12)

# Train the final Random Forest model using the full historical dataset
final_rf_model <- randomForest(Share ~ ., data = df %>% select(all_of(features), Share), ntree = 500, importance = TRUE)

# Predict 2025 ROY Share values for selected candidates
six_moy_2025$Predicted_Share <- predict(final_rf_model, newdata = six_moy_2025)

# Selecting top 5 predictions
predicted_six_moy_2025 <- six_moy_2025 %>%
  arrange(desc(Predicted_Share)) %>%  # Sort by Predicted_Share in descending order
  slice_head(n = 5)  # Select the top 5 players

# Print the top 5 predictions
predicted_six_moy_2025 %>% 
  select(c(player, Predicted_Share))

# Payton Pritchard will be the Sixth Man of the Year!











