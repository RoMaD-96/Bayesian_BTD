#   ____________________________________________________________________________
#   Libraries                                                               ####

packages <- c(
  "loo",
  "rstan",
  "cmdstanr",
  "bpcs",
  "readr",
  "dplyr",
  "tidyr",
  "bayesplot",
  "caret",
  "footBayes",
  "mlr3measures",
  "readxl"
)

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Load packages
invisible(lapply(packages, library, character.only = TRUE))


#   ____________________________________________________________________________
#   Bradley-Terry Ranking and Training Set                                  ####

# Load training data
load("Africa_Cup/Bayesian_BTD_Ranking/RData/ac_data_train.RData")

# Read and normalize FIFA rankings
fifa_ranking_clean <- read_excel("Data/ranking_fifa_dec_2023.xlsx")
fifa_ranking_clean$ranking <- (fifa_ranking_clean$ranking - median(fifa_ranking_clean$ranking)) / mad(fifa_ranking_clean$ranking)
rank <- fifa_ranking_clean

# Process dates in training data
times <- substr(ac_data_train$date, 1, 6)
times <- as.factor(times)
levels(times) <- 1:length(levels(times))
ac_data_train$date <- as.numeric(as.character(times))
ac_data_train <- arrange(ac_data_train, date)


#   ____________________________________________________________________________
#   Machine Learning Function                                             ####

ML_algorithms <- function(stage_train, stage_test) {
  
  # Random Forest
  random_forest <- train(
    outcome ~ rank1 + rank2,
    data = stage_train,
    method = "rf"
  )
  random_forest_pred <- predict(random_forest, stage_test)
  random_forest_pred_prob <- predict(random_forest, stage_test, type = "prob")
  random_forest_mat <- confusionMatrix(random_forest_pred, stage_test$outcome)
  brier_random_forest <- mbrier(stage_test$outcome, as.matrix(random_forest_pred_prob))
  
  # Bagged CART
  bag_CART <- train(
    outcome ~ rank1 + rank2,
    data = stage_train,
    method = "treebag"
  )
  bag_CART_pred <- predict(bag_CART, stage_test)
  bag_CART_pred_prob <- predict(bag_CART, stage_test, type = "prob")
  bag_CART_mat <- confusionMatrix(bag_CART_pred, stage_test$outcome)
  brier_bag_CART <- mbrier(stage_test$outcome, as.matrix(bag_CART_pred_prob))
  
  # MARS
  mars <- train(
    outcome ~ rank1 + rank2,
    data = stage_train,
    method = "gcvEarth"
  )
  mars_pred <- predict(mars, stage_test)
  mars_pred_prob <- predict(mars, stage_test, type = "prob")
  mars_mat <- confusionMatrix(mars_pred, stage_test$outcome)
  brier_mars <- mbrier(stage_test$outcome, as.matrix(mars_pred_prob))
  
  # Neural Network
  neural_net <- train(
    outcome ~ rank1 + rank2,
    data = stage_train,
    method = "nnet"
  )
  neural_net_pred <- predict(neural_net, stage_test)
  neural_net_pred_prob <- predict(neural_net, stage_test, type = "prob")
  neural_net_mat <- confusionMatrix(neural_net_pred, stage_test$outcome)
  brier_neural_net <- mbrier(stage_test$outcome, as.matrix(neural_net_pred_prob))
  
  # Collect accuracies
  accuracy <- c(
    random_forest_mat$overall["Accuracy"],
    bag_CART_mat$overall["Accuracy"],
    mars_mat$overall["Accuracy"],
    neural_net_mat$overall["Accuracy"]
  )
  
  # Return list of Brier scores and accuracies
  list_brier_acc <- list(
    brier_random_forest = brier_random_forest,
    brier_bag_CART = brier_bag_CART,
    brier_mars = brier_mars,
    brier_neural_net = brier_neural_net,
    accuracy = accuracy
  )
  
  return(list_brier_acc)
}


#   ____________________________________________________________________________
#   Machine Learning Algorithm                                              ####

##  ............................................................................
##  True Results                                                            ####

### True Results Match Day 1

ngames_matchday1 <- 12
ac_data_train_matchday1 <- data.frame(
  date = rep(length(levels(times)) + 1, ngames_matchday1),
  home_team = c(
    "Ivory Coast",
    "Nigeria",
    "Egypt",
    "Ghana",
    "Senegal",
    "Cameroon",
    "Algeria",
    "Burkina Faso",
    "Tunisia",
    "Mali",
    "Morocco",
    "DR Congo"
  ),
  away_team = c(
    "Guinea-Bissau",
    "Equatorial Guinea",
    "Mozambique",
    "Cape Verde",
    "Gambia",
    "Guinea",
    "Angola",
    "Mauritania",
    "Namibia",
    "South Africa",
    "Tanzania",
    "Zambia"
  ),
  home_score = c(2, 1, 2, 1, 3, 1, 1, 1, 0, 2, 3, 1),
  away_score = c(0, 1, 2, 2, 0, 1, 1, 0, 1, 0, 0, 1),
  tournament = rep("Africa Cup 2024", ngames_matchday1)
)

### True Results Match Day 2

ngames_matchday2 <- 12
ac_data_train_matchday2 <- data.frame(
  date = rep(length(levels(times)) + 1, ngames_matchday2),
  home_team = c(
    "Equatorial Guinea",
    "Ivory Coast",
    "Egypt",
    "Cape Verde",
    "Senegal",
    "Guinea",
    "Algeria",
    "Mauritania",
    "Tunisia",
    "South Africa",
    "Morocco",
    "Zambia"
  ),
  away_team = c(
    "Guinea-Bissau",
    "Nigeria",
    "Ghana",
    "Mozambique",
    "Cameroon",
    "Gambia",
    "Burkina Faso",
    "Angola",
    "Mali",
    "Namibia",
    "DR Congo",
    "Tanzania"
  ),
  home_score = c(4, 0, 2, 3, 3, 1, 2, 2, 1, 4, 1, 1),
  away_score = c(2, 1, 2, 0, 1, 0, 2, 3, 1, 0, 1, 1),
  tournament = rep("Africa Cup 2024", ngames_matchday2)
)

### True Results Match Day 3

ngames_matchday3 <- 12
ac_data_train_matchday3 <- data.frame(
  date = rep(length(levels(times)) + 1, ngames_matchday3),
  home_team = c(
    "Equatorial Guinea",
    "Guinea-Bissau",
    "Cape Verde",
    "Mozambique",
    "Gambia",
    "Guinea",
    "Angola",
    "Mauritania",
    "Namibia",
    "South Africa",
    "Tanzania",
    "Zambia"
  ),
  away_team = c(
    "Ivory Coast",
    "Nigeria",
    "Egypt",
    "Ghana",
    "Cameroon",
    "Senegal",
    "Burkina Faso",
    "Algeria",
    "Mali",
    "Tunisia",
    "DR Congo",
    "Morocco"
  ),
  home_score = c(4, 0, 2, 2, 2, 0, 2, 1, 0, 0, 0, 0),
  away_score = c(0, 1, 2, 2, 3, 2, 0, 0, 0, 0, 0, 1),
  tournament = rep("Africa Cup 2024", ngames_matchday3)
)

### Round of 16

ngames_matchday4 <- 8
ac_data_train_matchday4 <- data.frame(
  date = rep(length(levels(times)) + 2, ngames_matchday4),
  home_team = c(
    "Angola", "Nigeria",
    "Equatorial Guinea", "Egypt",
    "Cape Verde", "Senegal", 
    "Mali", "Morocco"
  ),
  away_team = c(
    "Namibia", "Cameroon",
    "Guinea", "DR Congo",
    "Mauritania", "Ivory Coast",
    "Burkina Faso", "South Africa"
  ),
  home_score = c(3, 2, 0, 1, 1, 1, 2, 0),
  away_score = c(0, 0, 1, 1, 0, 1, 1, 2),
  tournament = rep("Africa Cup 2024", ngames_matchday4)
)

### Quarter Finals

ngames_matchday5 <- 4
ac_data_train_matchday5 <- data.frame(
  date = rep(length(levels(times)) + 2, ngames_matchday5),
  home_team = c(
    "Nigeria", "DR Congo",
    "Mali", "Cape Verde"
  ),
  away_team = c(
    "Angola", "Guinea", 
    "Ivory Coast", "South Africa"
  ),
  home_score = c(1, 3, 1, 0),
  away_score = c(0, 1, 2, 0),
  tournament = rep("Africa Cup 2024", ngames_matchday5)
)

### Semi Finals

ngames_matchday6 <- 2        
ac_data_train_matchday6 <- data.frame(
  date = rep(length(levels(times)) + 2, ngames_matchday6),
  home_team = c("Nigeria", "Ivory Coast"),
  away_team = c("South Africa", "DR Congo"),
  home_score = c(1, 1),
  away_score = c(1, 0),
  tournament = rep("Africa Cup 2024", ngames_matchday6)
)

### Final

ngames_matchday7 <- 2        
ac_data_train_matchday7 <- data.frame(
  date = rep(length(levels(times)) + 2, ngames_matchday7),
  home_team = c("South Africa", "Nigeria"),
  away_team = c("DR Congo", "Ivory Coast"),
  home_score = c(0, 1),
  away_score = c(0, 2),
  tournament = rep("Africa Cup 2024", ngames_matchday7)
)


##  ............................................................................
##  Ranking                                                                 ####

# Add position to rankings
rank <- tibble::rowid_to_column(rank, "Position")

# Combine all match data into a single data frame
ac_data_train_ML <- rbind(
  ac_data_train[, -7],
  ac_data_train_matchday1,
  ac_data_train_matchday2,
  ac_data_train_matchday3,
  ac_data_train_matchday4,
  ac_data_train_matchday5,
  ac_data_train_matchday6,
  ac_data_train_matchday7
)

# Compute match outcomes: 1 = home win, 2 = draw, 3 = away win
ac_data_train_ML$outcome <- as.factor(ifelse(
  ac_data_train_ML$home_score > ac_data_train_ML$away_score, 1,
  ifelse(ac_data_train_ML$home_score < ac_data_train_ML$away_score, 3, 2)
))

# Relocate 'outcome' column before 'tournament'
ac_data_train_ML <- ac_data_train_ML %>% relocate(outcome, .before = tournament)

# Map team names to their rankings
ranking_lookup <- setNames(rank$ranking, rank$team_name)
ac_data_train_ML$rank1 <- ranking_lookup[ac_data_train_ML$home_team]
ac_data_train_ML$rank2 <- ranking_lookup[ac_data_train_ML$away_team]
ac_data_train_ML$rank_diff <- ac_data_train_ML$rank1 - ac_data_train_ML$rank2


##  ............................................................................
##  Group Stages                                                            ####

train_set_GS <- filter(ac_data_train_ML, date < 7) 
test_set_GS <- filter(ac_data_train_ML, date == 7)

print("Doing GS using FIFA")

set.seed(1111)
ML_pred_GS <- ML_algorithms(train_set_GS, test_set_GS)

save(ML_pred_GS, file = "Africa_Cup/Fifa_Ranking/RData/ML_pred_GS.RData")


##  ............................................................................
##  Knockout Stages                                                         ####

train_set_KS <- filter(ac_data_train_ML, date < 8)
test_set_KS <- filter(ac_data_train_ML, date == 8)

print("Doing KS using FIFA")

set.seed(1111)
ML_pred_KS <- ML_algorithms(train_set_KS, test_set_KS)

save(ML_pred_KS, file = "Africa_Cup/Fifa_Ranking/RData/ML_pred_KS.RData")

