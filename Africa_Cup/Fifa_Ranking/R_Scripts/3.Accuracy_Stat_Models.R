#   ____________________________________________________________________________
#   Library                                                                 ####

packages <- c(
  "rstan",
  "readr",
  "dplyr"
)


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
load("Africa_Cup/Unique_BT_Ranking/Simple_BT/RData/wc_data_train.RData")


times <- substr(wc_data_train$date, 1, 6)
times <- as.factor(times)
levels(times) <- c(1:length(levels(times)))
wc_data_train$date <- as.numeric(as.vector(times))
wc_data_train <- arrange(wc_data_train, date)

#   ____________________________________________________________________________
#   True Results                                                            ####

### MATCHDAY 1

### True Results Match Day 1

ngames_matchday1 <- 12
wc_data_train_matchday_1 <- data.frame(
  date = rep(length(levels(times)) + 1, ngames_matchday1),
  home_team = c(
    "Ivory Coast",
    "Nigeria" ,
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
wc_data_train_matchday2 <- data.frame(
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


### True Results Matchday 3
ngames_matchday3 <- 12
wc_data_train_matchday3 <- data.frame(
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
wc_data_train_matchday4 <- data.frame(
  date = rep(length(levels(times))+2, ngames_matchday4),
  home_team = c("Angola", "Nigeria",
                "Equatorial Guinea", "Egypt", "Cape Verde", "Senegal", 
                "Mali", "Morocco"),
  
  away_team = c("Namibia", "Cameroon", 
                "Guinea", "DR Congo", "Mauritania",
                "Ivory Coast", "Burkina Faso", "South Africa"),
  
  home_score = c(3,2,0,1,1,1,2,0),
  away_score = c(0,0,1,1,0,1,1,2),
  tournament = rep("Africa Cup 2024", ngames_matchday4))



### QUARTER FINALS

ngames_matchday5 <- 4
wc_data_train_matchday5 <- data.frame(
  date = rep(length(levels(times))+2, ngames_matchday5),
  home_team = c("Nigeria", "DR Congo",
                "Mali", "Cape Verde"),
  
  away_team = c("Angola", "Guinea", 
                "Ivory Coast", "South Africa"),
  
  home_score = c(1,3,1,0),
  away_score = c(0,1,2,0),
  tournament = rep("Africa Cup 2024", ngames_matchday5))

### SEMIFINALS

ngames_matchday6 <- 2        
wc_data_train_matchday6 <- data.frame(
  date = rep(length(levels(times))+2, ngames_matchday6),
  home_team = c( "Nigeria", "Ivory Coast"),
  away_team = c("South Africa" ,"DR Congo"  ),
  home_score = c(1,1),
  away_score = c(1,0),
  tournament = rep("Africa Cup 2024", ngames_matchday6))

### FINAL

ngames_matchday7 <- 2        
wc_data_train_matchday7 <- data.frame(
  date = rep(length(levels(times))+2, ngames_matchday7),
  home_team = c( "South Africa", "Nigeria"),
  away_team = c("DR Congo", "Ivory Coast"),
  home_score = c(0,1),
  away_score = c(0,2),
  tournament = rep("Africa Cup 2024", ngames_matchday7))




#   ____________________________________________________________________________
#   Bradley-Terry Ranking                                                   ####

print("Doing Accuracy e Brier Score Poisson Models with FIFA rank")

#   ____________________________________________________________________________
#   Group Stage                                                             ####

wc_data_train_GS <- rbind(wc_data_train_matchday_1,
                          wc_data_train_matchday2,
                          wc_data_train_matchday3)


load("Africa_Cup/Fifa_Ranking/RData/Stat_Model_GS.RData")

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

wc_data_train_KS <- rbind(wc_data_train_matchday4,
                          wc_data_train_matchday5,
                          wc_data_train_matchday6,
                          wc_data_train_matchday7)


load("Africa_Cup/Fifa_Ranking/RData/Stat_Model_KS.RData")


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
  file = "Africa_Cup/Fifa_Ranking/RData/Brier_GS_KS.RData"
)

### Accuracy GS KS 

acc_models_GS_KS <- list( GroupStage = acc_groupstage_GS,
                               knockoutStage = acc_groupstage_KS)
save(
  acc_models_GS_KS,
  acc_groupstage_GS,
  acc_groupstage_KS,
  file = "Africa_Cup/Fifa_Ranking/RData/Accuracy_GS_KS.RData"
)


