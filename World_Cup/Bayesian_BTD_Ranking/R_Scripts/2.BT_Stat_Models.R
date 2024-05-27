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
  "footBayes")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))



rstan_options(auto_write = TRUE)
options(mc.cores = 4)

#   ____________________________________________________________________________
#   Bradley-Terry Ranking and Training Set                                  ####

load("World_Cup/Bayesian_BTD_Ranking/RData/rank_bradley_terry.RData")
load("World_Cup/Bayesian_BTD_Ranking/RData/wc_data_train.RData")

rank <- as.data.frame(rank)

# MAD Normalization
rank$Median <- (rank$Median - median(rank$Median))/(mad(rank$Median))


times <- substr(wc_data_train$date, 1, 4)
times <- as.factor(times)
levels(times) <- c(1:length(levels(times)))
wc_data_train$date <- as.numeric(as.vector(times))
wc_data_train <- arrange(wc_data_train, date)

#   ____________________________________________________________________________
#   Bayesian Statistical Models                                             ####

##  ............................................................................
##  Group Stage                                                             ####

ngames_prev <- 48
wc_data_test <- data.frame(
  date = rep(length(levels(times)) + 1, ngames_prev),
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
    "Brazil",
    "Wales",
    "Qatar",
    "Netherlands",
    "England",
    "Tunisia",
    "Poland",
    "France",
    "Argentina",
    "Japan",
    "Germany",
    "Belgium",
    "Croatia",
    "Cameroon",
    "Brazil",
    "Portugal",
    "South Korea",
    "Ecuador",
    "Netherlands",
    "Iran",
    "Wales",
    "Tunisia",
    "Australia",
    "Poland",
    "Saudi Arabia",
    "Croatia",
    "Canada",
    "Japan",
    "Costa Rica",
    "South Korea",
    "Ghana",
    "Serbia",
    "Cameroon"
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
    "Serbia",
    "Iran",
    "Senegal",
    "Ecuador",
    "United States",
    "Australia",
    "Saudi Arabia",
    "Denmark",
    "Mexico",
    "Costa Rica",
    "Spain",
    "Morocco",
    "Canada",
    "Serbia",
    "Switzerland",
    "Uruguay",
    "Ghana",
    "Senegal",
    "Qatar",
    "United States",
    "England",
    "France",
    "Denmark",
    "Argentina",
    "Mexico",
    "Belgium",
    "Morocco",
    "Spain",
    "Germany",
    "Portugal",
    "Uruguay",
    "Switzerland",
    "Brazil"
  ), 
  
  home_score = rep(NA, ngames_prev),
  away_score = rep(NA, ngames_prev),
  tournament = rep(NA, ngames_prev))

wc_data_stan <-rbind(wc_data_train[,-7], wc_data_test)

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Dynamic Models                                                          ####

print("Doing Poisson Models WC")

### Diagonal Inflated Bivariate Poisson

set.seed(4231)

n_iter <- 2000
diag_inf_biv_pois_GS <- stan_foot(data = wc_data_stan[,-6],
                                  model="diag_infl_biv_pois",
                                  iter = n_iter, cores = 4,
                                  predict= ngames_prev,
                                  ranking = rank[,c(1,3)],
                                  dynamic_type = "seasonal",
                                  ind_home = "FALSE") # diag. inflated biv pois



### Bivariate Poisson
set.seed(4231)

n_iter <- 2000
biv_pois_GS <- stan_foot(data = wc_data_stan[,-6],
                         model = "biv_pois",
                         iter = n_iter, cores = 4,
                         predict= ngames_prev,
                         ranking = rank[,c(1,3)],
                         dynamic_type = "seasonal",
                         ind_home = "FALSE") 



### Double Poisson

set.seed(4231)

n_iter <- 2000
double_pois_GS <- stan_foot(data = wc_data_stan[,-6],
                            model = "double_pois",
                            iter = n_iter, cores = 4,
                            predict= ngames_prev,
                            ranking = rank[,c(1,3)],
                            dynamic_type = "seasonal",
                            ind_home = "FALSE") 


save(diag_inf_biv_pois_GS, biv_pois_GS, double_pois_GS, file = "World_Cup/Bayesian_BTD_Ranking/RData/Stat_Model_GS.RData")

rm(diag_inf_biv_pois_GS, biv_pois_GS, double_pois_GS)
gc()

##  ............................................................................
##  Knockout Stages                                                         ####

### True Results Match Day 1

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

### True Results Match Day 2

ngames_matchday2 <- 16
wc_data_train_matchday2 <- data.frame(
  date = rep(length(levels(times)) + 1, ngames_matchday2),
  home_team = c(
    "Wales",
    "Qatar",
    "Netherlands",
    "England",
    "Tunisia",
    "Poland",
    "France",
    "Argentina",
    "Japan",
    "Germany",
    "Belgium",
    "Croatia",
    "Cameroon",
    "Brazil",
    "Portugal",
    "South Korea"
  ),
  
  away_team = c(
    "Iran",
    "Senegal",
    "Ecuador",
    "United States",
    "Australia",
    "Saudi Arabia",
    "Denmark",
    "Mexico",
    "Costa Rica",
    "Spain",
    "Morocco",
    "Canada",
    "Serbia",
    "Switzerland",
    "Uruguay",
    "Ghana"
  ),
  
  home_score = c(0, 1, 1, 0, 0, 2, 2, 2, 0, 1, 0, 4, 3, 1, 2, 2),
  away_score = c(2, 3, 1, 0, 1, 0, 1, 0, 1, 1, 2, 1, 3, 0, 0, 3),
  tournament = rep("World Cup 2022", ngames_matchday2)
)


### True Results Matchday 3
ngames_matchday3 <- 16
wc_data_train_matchday3 <- data.frame(
  date = rep(length(levels(times)) + 1, ngames_matchday3),
  home_team = c(
    "Ecuador",
    "Netherlands",
    "Iran",
    "Wales",
    "Tunisia",
    "Australia",
    "Poland",
    "Saudi Arabia",
    "Croatia",
    "Canada",
    "Japan",
    "Costa Rica",
    "South Korea",
    "Ghana",
    "Serbia",
    "Cameroon"
  ),
  
  away_team = c(
    "Senegal",
    "Qatar",
    "United States",
    "England",
    "France",
    "Denmark",
    "Argentina",
    "Mexico",
    "Belgium",
    "Morocco",
    "Spain",
    "Germany",
    "Portugal",
    "Uruguay",
    "Switzerland",
    "Brazil"
  ),
  
  home_score = c(1, 2, 0, 0, 1, 1, 0, 1, 0, 1, 2, 2, 2, 0, 2, 1),
  away_score = c(2, 0, 1, 3, 0, 0, 2, 2, 0, 2, 1, 4, 1, 2, 3, 0),
  tournament = rep("World Cup 2022", ngames_matchday3)
)

### Train Test Knockout
ngames_knockout <- 16
wc_data_test <- data.frame(
  date = rep(length(levels(times))+2, ngames_knockout),
  home_team = c("Netherlands", "Argentina", 
                "France","England",
                "Japan","Brazil" ,
                "Morocco", "Portugal","Croatia", "Netherlands",
                "Morocco", "England","Argentina", "France","Argentina", "Croatia"),
  
  away_team = c( "United States", "Australia",
                 "Poland", "Senegal",
                 "Croatia", "South Korea",
                 "Spain", "Switzerland","Brazil", "Argentina", 
                 "Portugal", "France","Croatia" ,"Morocco","France" ,"Morocco"),
  
  home_score = rep(NA, ngames_knockout),
  away_score = rep(NA, ngames_knockout),
  tournament = rep(NA,ngames_knockout))

wc_data_stan <-rbind(wc_data_train[,-7],
                     wc_data_train_matchday_1,
                     wc_data_train_matchday2,
                     wc_data_train_matchday3,
                     wc_data_test)


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Dynamic Models                                                          ####


### Diagonal Inflated Bivariate Poisson

set.seed(4231)

n_iter <- 2000
diag_inf_biv_pois_KS <- stan_foot(data = wc_data_stan[,-6],
                                  model="diag_infl_biv_pois",
                                  iter = n_iter, cores = 4,
                                  predict= ngames_knockout,
                                  ranking = rank[,c(1,3)],
                                  dynamic_type = "seasonal",
                                  ind_home = "FALSE") # diag. inflated biv pois



### Bivariate Poisson

set.seed(4231)

n_iter <- 2000
biv_pois_KS <- stan_foot(data = wc_data_stan[,-6],
                         model = "biv_pois",
                         iter = n_iter, cores = 4,
                         predict= ngames_knockout,
                         ranking = rank[,c(1,3)],
                         dynamic_type = "seasonal",
                         ind_home = "FALSE") 



### Double Poisson

set.seed(4231)

n_iter <- 2000
double_pois_KS <- stan_foot(data = wc_data_stan[,-6],
                            model = "double_pois",
                            iter = n_iter, cores = 4,
                            predict= ngames_knockout,
                            ranking = rank[,c(1,3)],
                            dynamic_type = "seasonal",
                            ind_home = "FALSE") 



save(diag_inf_biv_pois_KS, biv_pois_KS, double_pois_KS, file = "World_Cup/Bayesian_BTD_Ranking/RData/Stat_Model_KS.RData")
