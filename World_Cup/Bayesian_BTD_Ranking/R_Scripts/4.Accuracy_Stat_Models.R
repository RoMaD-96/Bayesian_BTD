#   ____________________________________________________________________________
#   Library                                                                 ####

packages <- c(
  "loo",
  "rstan",
  "caret",
  "bpcs",
  "readr",
  "dplyr",
  "tidyr",
  "footBayes")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

set.seed(433)
rstan_options(auto_write = TRUE)
options(mc.cores = 4)

#   ____________________________________________________________________________
#   Accuracy Function                                                      ####

source("Useful_Functions.R")
load("World_Cup/Bayesian_BTD_Ranking/RData/wc_data_train.RData")

times <- substr(wc_data_train$date, 1, 4)
times <- as.factor(times)
levels(times) <- c(1:length(levels(times)))
wc_data_train$date <- as.numeric(as.vector(times))
wc_data_train <- arrange(wc_data_train, date)

#   ____________________________________________________________________________
#   True Results                                                            ####


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
  date = rep(length(levels(times))+3, ngames_matchday6),
  home_team = c( "Argentina", "France"  ),
  away_team = c("Croatia" ,"Morocco"  ),
  home_score = c(3,2),
  away_score = c(0,0),
  tournament =  rep("World Cup 2022", ngames_matchday6))

### FINAL

ngames_matchday7 <- 2        
wc_data_train_matchday7 <- data.frame(
  date = rep(length(levels(times))+4, ngames_matchday7),
  home_team = c( "Argentina", "Croatia"),
  away_team = c("France", "Morocco"),
  home_score = c(2,2),
  away_score = c(2,1),
  tournament =  rep("World Cup 2022", ngames_matchday7))



#   ____________________________________________________________________________
#   Bradley-Terry Ranking                                                   ####

print("Computing Brier Score and Accuracy Group Stage BT")

#   ____________________________________________________________________________
#   Group Stage                                                             ####

wc_data_train_GS <- rbind(wc_data_train_matchday_1,
                          wc_data_train_matchday2,
                          wc_data_train_matchday3)


load("World_Cup/Bayesian_BTD_Ranking/RData/Stat_Model_GS.RData")

acc_groupstage_GS <- acc_stan(diag_inf_biv_pois_GS,
                                   biv_pois_GS,
                                   double_pois_GS,
                                   wc_data_train_GS)

brier_groupstage_GS <- brier_score(diag_inf_biv_pois_GS,
                                        biv_pois_GS,
                                        double_pois_GS,
                                        wc_data_train_GS)


#   ____________________________________________________________________________
#   Knockout Stage                                                             ####

print("Computing Brier Score and Accuracy Knockout Stage BT")


wc_data_train_KS <- rbind(wc_data_train_matchday4,
                          wc_data_train_matchday5,
                          wc_data_train_matchday6,
                          wc_data_train_matchday7)


load("World_Cup/Bayesian_BTD_Ranking/RData/Stat_Model_KS.RData")


acc_groupstage_KS <- acc_stan(diag_inf_biv_pois_KS,
                                   biv_pois_KS,
                                   double_pois_KS,
                                   wc_data_train_KS)

brier_groupstage_KS <- brier_score(diag_inf_biv_pois_KS,
                                        biv_pois_KS,
                                        double_pois_KS,
                                        wc_data_train_KS)

### Brier GS KS 
tab_brier_GS_KS <-
  cbind(
    lapply(brier_groupstage_GS, round,3),
    lapply(brier_groupstage_KS, round,3)
  )

tab_brier_GS_KS <- as.data.frame(tab_brier_GS_KS)
rownames(tab_brier_GS_KS) <-
  c("Diag. Infl. Pois. (BT Rank)",
    "Biv. Pois. (BT Rank)",
    "Double Pois. (BT Rank)")
colnames(tab_brier_GS_KS) <-
  c("Group Stage",
    "knockout Stage")

save(
  tab_brier_GS_KS,
  brier_groupstage_GS,
  brier_groupstage_KS,
  file = "World_Cup/Bayesian_BTD_Ranking/RData/Brier_GS_KS.RData"
)

### Accuracy GS KS 

acc_models_GS_KS <- list( GroupStage = acc_groupstage_GS,
                               knockoutStage = acc_groupstage_KS)
save(
  acc_models_GS_KS,
  acc_groupstage_GS,
  acc_groupstage_KS,
  file = "World_Cup/Bayesian_BTD_Ranking/RData/Accuracy_GS_KS.RData"
)

