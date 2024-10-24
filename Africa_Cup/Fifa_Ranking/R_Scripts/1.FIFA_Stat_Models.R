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
  "readxl"
)

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(!installed_packages)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

set.seed(4231)
rstan_options(auto_write = TRUE)
options(mc.cores = 4)

#   ____________________________________________________________________________
#   FIFA Ranking and Training Set                                  ####

load("Africa_Cup/Bayesian_BTD_Ranking/RData/ac_data_train.RData")
load("Africa_Cup/Bayesian_BTD_Ranking/RData/rank_bradley_terry.RData")

fifa_ranking_clean <- read_excel("Data/ranking_fifa_dec_2023.xlsx")

# MAD normalization
fifa_ranking_clean$ranking <- (fifa_ranking_clean$ranking - median(fifa_ranking_clean$ranking)) / mad(fifa_ranking_clean$ranking)

ranking <- fifa_ranking_clean %>%
  filter(team_name %in% rank$Parameter)
rank <- as.data.frame(ranking)

times <- substr(ac_data_train$date, 1, 6)
times <- as.factor(times)
levels(times) <- 1:length(levels(times))
ac_data_train$date <- as.numeric(as.vector(times))
ac_data_train <- arrange(ac_data_train, date)

#   ____________________________________________________________________________
#   Bayesian Statistical Models                                             ####

##  ............................................................................
##  Group Stage                                                             ####

ngames_prev <- 36
ac_data_test <- data.frame(
  date = rep(length(levels(times)) + 1, ngames_prev),
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
    "DR Congo",
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
    "Zambia",
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
    "Zambia",
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
    "Tanzania",
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
  home_score = rep(NA, ngames_prev),
  away_score = rep(NA, ngames_prev),
  tournament = rep(NA, ngames_prev)
)

ac_data_stan <- rbind(ac_data_train[, -7], ac_data_test)

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Dynamic Models                                                          ####

print("Doing Poisson Models with FIFA Ranking")

### Diagonal Inflated Bivariate Poisson

set.seed(4231)
n_iter <- 2000
diag_inf_biv_pois_GS <- stan_foot(
  data = ac_data_stan[, -6],
  model = "diag_infl_biv_pois",
  iter = n_iter,
  cores = 4,
  predict = ngames_prev,
  ranking = rank[, c(1, 2)],
  dynamic_type = "seasonal",
  ind_home = "FALSE"
)

### Bivariate Poisson

set.seed(4231)
n_iter <- 2000
biv_pois_GS <- stan_foot(
  data = ac_data_stan[, -6],
  model = "biv_pois",
  iter = n_iter,
  cores = 4,
  predict = ngames_prev,
  ranking = rank[, c(1, 2)],
  dynamic_type = "seasonal",
  ind_home = "FALSE"
)

### Double Poisson

set.seed(4231)
n_iter <- 2000
double_pois_GS <- stan_foot(
  data = ac_data_stan[, -6],
  model = "double_pois",
  iter = n_iter,
  cores = 4,
  predict = ngames_prev,
  ranking = rank[, c(1, 2)],
  dynamic_type = "seasonal",
  ind_home = "FALSE"
)

save(
  diag_inf_biv_pois_GS,
  biv_pois_GS,
  double_pois_GS,
  file = "Africa_Cup/Fifa_Ranking/RData/Stat_Model_GS.RData"
)

##  ............................................................................
##  Knockout Stages                                                         ####

### True Results Match Day 1

ngames_matchday1 <- 12
ac_data_train_matchday_1 <- data.frame(
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
ac_data_train_matchday_2 <- data.frame(
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
ac_data_train_matchday_3 <- data.frame(
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

### Train Test Knockout

ngames_knockout <- 16
ac_data_test <- data.frame(
  date = rep(length(levels(times)) + 2, ngames_knockout),
  home_team = c(
    "Angola", "Nigeria", "Equatorial Guinea", "Egypt",
    "Cape Verde", "Senegal", "Mali", "Morocco",
    "Nigeria", "DR Congo", "Mali", "Cape Verde",
    "Nigeria", "Ivory Coast", "South Africa", "Nigeria"
  ),
  away_team = c(
    "Namibia", "Cameroon", "Guinea", "DR Congo",
    "Mauritania", "Ivory Coast", "Burkina Faso", "South Africa",
    "Angola", "Guinea", "Ivory Coast", "South Africa",
    "South Africa", "DR Congo", "DR Congo", "Ivory Coast"
  ),
  home_score = rep(NA, ngames_knockout),
  away_score = rep(NA, ngames_knockout),
  tournament = rep(NA, ngames_knockout)
)

ac_data_stan <- rbind(
  ac_data_train[, -7],
  ac_data_train_matchday_1,
  ac_data_train_matchday_2,
  ac_data_train_matchday_3,
  ac_data_test
)

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Dynamic Models                                                          ####

print("Starting Poisson Models with FIFA Ranking for Africa Cup")

### Diagonal Inflated Bivariate Poisson

set.seed(4231)
n_iter <- 2000
diag_inf_biv_pois_KS <- stan_foot(
  data = ac_data_stan[, -6],
  model = "diag_infl_biv_pois",
  iter = n_iter,
  cores = 4,
  predict = ngames_knockout,
  ranking = rank[, c(1, 2)],
  dynamic_type = "seasonal",
  ind_home = "FALSE"
)

### Bivariate Poisson

set.seed(4231)
n_iter <- 2000
biv_pois_KS <- stan_foot(
  data = ac_data_stan[, -6],
  model = "biv_pois",
  iter = n_iter,
  cores = 4,
  predict = ngames_knockout,
  ranking = rank[, c(1, 2)],
  dynamic_type = "seasonal",
  ind_home = "FALSE"
)

### Double Poisson

set.seed(4231)
n_iter <- 2000
double_pois_KS <- stan_foot(
  data = ac_data_stan[, -6],
  model = "double_pois",
  iter = n_iter,
  cores = 4,
  predict = ngames_knockout,
  ranking = rank[, c(1, 2)],
  dynamic_type = "seasonal",
  ind_home = "FALSE"
)

save(
  diag_inf_biv_pois_KS,
  biv_pois_KS,
  double_pois_KS,
  file = "Africa_Cup/Fifa_Ranking/RData/Stat_Model_KS.RData"
)
