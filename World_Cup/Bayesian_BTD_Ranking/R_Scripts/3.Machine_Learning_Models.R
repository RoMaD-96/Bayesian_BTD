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
  "mlr3measures")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))


#   ____________________________________________________________________________
#   Bradley-Terry Ranking and Training Set                                  ####

load("World_Cup/Bayesian_BTD_Ranking/RData/rank_bradley_terry.RData")
load("World_Cup/Bayesian_BTD_Ranking/RData/wc_data_train.RData")

# MAD Normalization
rank$Median <- (rank$Median - median(rank$Median))/(mad(rank$Median))


times <- #paste(
  substr(wc_data_train$date, 1, 4)
times <- as.factor(times)
levels(times) <- c(1:length(levels(times)))
wc_data_train$date <- as.numeric(as.vector(times))
wc_data_train <- arrange(wc_data_train, date)

#   ____________________________________________________________________________
#   Machine Learning Function                                             ####


ML_algorithms <- function(stage_train, stage_test){
  
  # random forest
  random_forest <- train(as.factor(outcome) ~ rank1+rank2, 
               data = stage_train,
               method = "rf")
  random_forest_pred <- predict(random_forest, stage_test)
  random_forest_pred_prob <- predict(random_forest, stage_test, type = "prob")
  
  random_forest_mat <- confusionMatrix(random_forest_pred, as.factor(stage_test$outcome))
  
  brier_random_forest <- mbrier(as.factor(stage_test$outcome), as.matrix(random_forest_pred_prob))
  
  
  # Bagged CART
  bag_CART <- train(as.factor(outcome) ~ rank1+rank2, 
               data = stage_train, 
               method = "treebag")
  bag_CART_pred <- predict(bag_CART, stage_test)
  bag_CART_pred_prob <- predict(bag_CART, stage_test, type = "prob")
  bag_CART_mat <- confusionMatrix(bag_CART_pred, as.factor(stage_test$outcome))
  
  brier_bag_CART <- mbrier(as.factor(stage_test$outcome), as.matrix(bag_CART_pred_prob))
  

  # MARS
  mars<- train(as.factor(outcome) ~ rank1+rank2, 
              data = stage_train, 
              method = "gcvEarth")
  mars_pred <- predict(mars, stage_test)
  mars_pred_prob <- predict(mars, stage_test, type = "prob")
  mars_mat <- confusionMatrix(mars_pred, as.factor(stage_test$outcome))
  
  brier_mars <- mbrier(as.factor(stage_test$outcome), as.matrix(mars_pred_prob))
  
  
  # Neural network
  neural_net<- train(as.factor(outcome) ~ rank1+rank2, 
              data = stage_train, 
              method = "nnet")
  neural_net_pred <- predict(neural_net, stage_test)
  neural_net_pred_prob <- predict(neural_net, stage_test, type = "prob")
  neural_net_mat <- confusionMatrix(neural_net_pred, as.factor(stage_test$outcome))
  accuracy <- as.vector(c(random_forest_mat$overall[1], bag_CART_mat$overall[1],
                          mars_mat$overall[1], neural_net_mat$overall[1]))
  
  brier_neural_net <- mbrier(as.factor(stage_test$outcome), as.matrix(neural_net_pred_prob))
  
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

### MATCHDAY 1

ngames_matchday1 <- 16
wc_data_train_matchday_1 <- data.frame(
  date = rep(length(levels(times)) + 1, ngames_matchday1),
  home_team = c(
    "Qatar",
    "England" ,
    "Senegal",
    "United States",
    "Argentina",
    "Denmark",
    "Mexico",
    "France",
    "Morocco",
    "Germany",
    "Spain",
    "Belgium",
    "Switzerland",
    "Uruguay",
    "Portugal",
    "Brazil"
  ),
  
  away_team = c(
    "Ecuador",
    "Iran",
    "Netherlands",
    "Wales",
    "Saudi Arabia",
    "Tunisia",
    "Poland",
    "Australia",
    "Croatia",
    "Japan",
    "Costa Rica",
    "Canada",
    "Cameroon",
    "South Korea",
    "Ghana",
    "Serbia"
  ),
  
  home_score = c(0, 6, 0, 1, 1, 0, 0, 4, 0, 1, 7, 1, 1, 0, 3, 2),
  away_score = c(2, 2, 2, 1, 2, 0, 0, 1, 0, 2, 0, 0, 0, 0, 2, 0),
  tournament = rep("World Cup 2022", ngames_matchday1)
)

### MATCHDAY 2

ngames_matchday2 <- 16
wc_data_train_matchday2 <- data.frame(
  date = rep(length(levels(times))+1, ngames_matchday2),
  home_team = c("Wales","Qatar","Netherlands",
                "England", "Tunisia","Poland",
                "France","Argentina", "Japan","Germany",
                "Belgium","Croatia","Cameroon","Brazil",
                "Portugal","South Korea"),
  
  away_team = c( "Iran",  "Senegal","Ecuador",
                 "United States", "Australia",
                 "Saudi Arabia",
                 "Denmark","Mexico","Costa Rica","Spain",
                 "Morocco","Canada","Serbia","Switzerland",
                 "Uruguay","Ghana"),
  
  home_score = c(0,1,1,0,0,2,2,2,0,1,0,4,3,1,2,2),
  away_score = c(2,3,1,0,1,0,1,0,1,1,2,1,3,0,0,3),
  tournament = rep("World Cup 2022",ngames_matchday2))


### MATCHDAY 3

ngames_matchday3 <- 16
wc_data_train_matchday3 <- data.frame(
  date = rep(length(levels(times))+1, ngames_matchday3),
  home_team = c("Ecuador", "Netherlands", 
                "Iran", "Wales",
                "Tunisia", "Australia",
                "Poland", "Saudi Arabia",
                "Croatia", "Canada",
                "Japan", "Costa Rica",
                "South Korea", "Ghana",
                "Serbia","Cameroon"),
  
  away_team = c( "Senegal", "Qatar",
                 "United States",  "England",
                 "France", "Denmark",
                 "Argentina", "Mexico",
                 "Belgium", "Morocco",
                 "Spain",  "Germany",
                 "Portugal", "Uruguay",
                 "Switzerland", "Brazil"),
  
  home_score = c(1,2,0,0,1,1,0,1,0,1,2,2,2,0,2,1),
  away_score = c(2,0,1,3,0,0,2,2,0,2,1,4,1,2,3,0),
  tournament = rep("World Cup 2022", ngames_matchday3))

ngames_matchday3 <- 16
wc_data_train_matchday3 <- data.frame(
  date = rep(length(levels(times))+1, ngames_matchday3),
  home_team = c("Ecuador", "Netherlands", 
                "Iran", "Wales",
                "Tunisia", "Australia",
                "Poland", "Saudi Arabia",
                "Croatia", "Canada",
                "Japan", "Costa Rica",
                "South Korea", "Ghana",
                "Serbia","Cameroon"),
  
  away_team = c( "Senegal", "Qatar",
                 "United States",  "England",
                 "France", "Denmark",
                 "Argentina", "Mexico",
                 "Belgium", "Morocco",
                 "Spain",  "Germany",
                 "Portugal", "Uruguay",
                 "Switzerland", "Brazil"),
  
  home_score = c(1,2,0,0,1,1,0,1,0,1,2,2,2,0,2,1),
  away_score = c(2,0,1,3,0,0,2,2,0,2,1,4,1,2,3,0),
  tournament = rep("World Cup 2022", ngames_matchday3))

### ROUND OF 16

ngames_matchday4 <- 8
wc_data_train_matchday4 <- data.frame(
  date = rep(length(levels(times))+2, ngames_matchday4),
  home_team = c("Netherlands", "Argentina", 
                "France","England",
                "Japan","Brazil" ,
                "Morocco", "Portugal"),
  
  away_team = c( "United States", "Australia",
                 "Poland", "Senegal",
                 "Croatia", "South Korea",
                 "Spain", "Switzerland"),
  
  home_score = c(3,2,3,3,1,4,0,6),
  away_score = c(1,1,1,0,1,1,0,1),
  tournament = rep("World Cup 2022", ngames_matchday4))

### QUARTER FINALS

ngames_matchday5 <- 4
wc_data_train_matchday5 <- data.frame(
  date = rep(length(levels(times))+2, ngames_matchday5),
  home_team = c("Croatia", "Netherlands",
                "Morocco", "England"),
  
  away_team = c("Brazil", "Argentina", 
                "Portugal", "France"),
  
  home_score = c(1,2,1,1),
  away_score = c(1,2,0,2),
  tournament = rep("World Cup 2022", ngames_matchday5))

### SEMIFINALS

ngames_matchday6 <- 2        
wc_data_train_matchday6 <- data.frame(
  date = rep(length(levels(times))+2, ngames_matchday6),
  home_team = c( "Argentina", "France"  ),
  away_team = c("Croatia" ,"Morocco"  ),
  home_score = c(3,2),
  away_score = c(0,0),
  tournament =  rep("World Cup 2022", ngames_matchday6))

### FINAL

ngames_matchday7 <- 2        
wc_data_train_matchday7 <- data.frame(
  date = rep(length(levels(times))+2, ngames_matchday7),
  home_team = c( "Argentina", "Croatia"),
  away_team = c("France", "Morocco"),
  home_score = c(2,2),
  away_score = c(2,1),
  tournament =  rep("World Cup 2022", ngames_matchday7))

##  ............................................................................
##  Ranking                                                                 ####


rank <- tibble::rowid_to_column(rank, "Position")
wc_data_train_ML <-
  rbind(
    wc_data_train[, -7],
    wc_data_train_matchday_1,
    wc_data_train_matchday2,
    wc_data_train_matchday3,
    wc_data_train_matchday4,
    wc_data_train_matchday5,
    wc_data_train_matchday6,
    wc_data_train_matchday7
  )

outcome <- c(rep(NA,length(wc_data_train_ML$home_team)))

for (i in 1:length(wc_data_train_ML$home_team)) {
  if (wc_data_train_ML$home_score[i] > wc_data_train_ML$away_score[i]){
    outcome[i] <- 1
  }
  else if (wc_data_train_ML$home_score[i] < wc_data_train_ML$away_score[i]){
    outcome[i] <- 3
  }
  else {
    outcome[i] <- 2
  }
}
outcome <- as.factor(outcome)
wc_data_train_ML <- as_tibble(cbind(wc_data_train_ML,outcome))
wc_data_train_ML <- wc_data_train_ML %>% relocate(outcome, .before=tournament)


teams <- unique(wc_data_train_ML$home_team)
team_home <- match( wc_data_train_ML$home_team, teams)
team_away <- match( wc_data_train_ML$away_team, teams)
team1 <- team_home[1:length(wc_data_train_ML$outcome)]
team2 <- team_away[1:length(wc_data_train_ML$outcome)]
ranking1 <- match(wc_data_train_ML$home_team, rank$Parameter)
ranking2 <- match(wc_data_train_ML$away_team, rank$Parameter)

rank1 <- rank2 <- c()
for (n in 1:length(team1)){
  #rank_diff[n] <- ranking[team1[1]]-ranking[team2[n]]
  rank1[n] <- rank$Median[ranking1[n]]
  rank2[n] <- rank$Median[ranking2[n]]
}
rank_diff <- rank1-rank2

wc_data_train_ML <- cbind(wc_data_train_ML,rank1,rank2,rank_diff)

##  ............................................................................
##  Group Stages                                                            ####

train_set_GS <- wc_data_train_ML[-c(3215:3278),]
test_set_GS <- wc_data_train_ML[c(3215:3262),]

print("Doing GS using BT")

set.seed(4321)
ML_groupstage_GS <- ML_algorithms(train_set_GS, test_set_GS)

save(ML_groupstage_GS, file = "World_Cup/Bayesian_BTD_Ranking/RData/ML_groupstage_GS.RData")


##  ............................................................................
##  Knockout Stages                                                         ####

train_set_KS <- wc_data_train_ML[-c(3263:3278),]
test_set_KS <- wc_data_train_ML[c(3263:3278),]

print("Doing KS using BT")

set.seed(451)
ML_groupstage_KS <- ML_algorithms(train_set_KS, test_set_KS)

save(ML_groupstage_KS, file = "World_Cup/Bayesian_BTD_Ranking/RData/ML_groupstage_KS.RData")

