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
  "readxl")

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
#   Data                                                                    ####

wc_data <- read.csv2(file = "Data/results_Africa_Cup.csv", sep = ",")
wc_data_train <- wc_data  %>%
  select(date, home_team, away_team,
         home_score, away_score, tournament) %>%
  filter(as.Date(wc_data$date) > "2018-08-01" & as.Date(wc_data$date) < "2023-12-22
")

# Fifa Ranking

fifa_ranking_clean <- read_excel("Data/ranking_fifa_dec_2023.xlsx")


# Redefining times from 1 to 6

times <- as.factor(substr(wc_data_train$date, 1, 5))
levels(times) <- c(1:length(levels(times)))
wc_data_train$date <- as.numeric(as.vector(times))
wc_data_train <- arrange(wc_data_train, date)


#  ____________________________________________________________________________
#   Training Set                                                            ####

wc_data_train <- wc_data_train %>%
  filter(!is.na(wc_data_train$home_score) &
           !is.na(wc_data_train$away_score))

low_teams <- c(
  "Haiti",
  "Sint Maarten",
  "CuraÃ§ao",
  "Grenada",
  "Cuba",
  "Turks and Caicos Islands",
  "Jersey",
  "Hitra",
  "Isle of Man",
  "Yorkshire",
  "Panjab",
  "Somaliland",
  "Kernow",
  "Barawa",
  "Chagos Islands",
  "Cascadia",
  "Parishes of Jersey",
  "Alderney",
  "Yoruba Nation",
  "Matabeleland",
  "Biafra",
  "Mapuche",
  "Maule Sur",
  "Aymara",
  "Saint Helena",
  "Shetland",
  "Ynys MÃ´n",
  "Orkney",
  "Guernsey",
  "Western Isles"
)

other_discarded_teams <- wc_data_train %>% filter(!(home_team %in% fifa_ranking_clean$team_name))
teams_removed <- c(low_teams, other_discarded_teams$home_team)
wc_1 <- wc_data_train %>% filter(!(home_team %in% teams_removed))
wc_2 <- wc_1 %>% filter(!(away_team %in% teams_removed))
wc_data_train <- wc_2


# Final outcome 

match_outcome <- c(rep(NA,length(wc_data_train$home_team)))

for (i in 1:length(wc_data_train$home_team)) {
  if (wc_data_train$home_score[i] > wc_data_train$away_score[i]){
    match_outcome[i] <- 1
  }
  else if (wc_data_train$home_score[i] < wc_data_train$away_score[i]){
    match_outcome[i] <- 3
  }
  else {
    match_outcome[i] <- 2
  }
}


#   ____________________________________________________________________________
#   Final Training Set                                                      ####


wc_data_train <- as_tibble(cbind(wc_data_train,match_outcome))


#   ____________________________________________________________________________
#   Bradley-Terry Model                                                     ####

rank_model <- bpc(
  wc_data_train,
  player0 = 'away_team',
  player1 =  'home_team',
  player0_score = 'away_score',
  player1_score = 'home_score',
  model_type = 'davidson',
  solve_ties = 'none',
  priors = list(prior_lambda_std = 5.0,
                prior_lambda_mu = 2.0),
  iter = 4000,
  show_chain_messages = TRUE
)


print(rank_model,
      credMass = 0.95, diagnostics = FALSE)

lambda_table <- get_parameters(rank_model, "lambda", keep_par_name = F)

# fit_rank <- get_fit(rank_model)
# posterior_draws <- posterior::as_draws_matrix(fit_rank$draws())
# bayesplot::mcmc_trace(posterior_draws,pars = c("lambda[1]","lambda[2]","lambda[3]","lambda[195]"), n_warmup=1000)
#rank <- get_rank_of_players_df(rank_model, n = 2000)

lambda_table <- as.data.frame(lambda_table)
rank <- lambda_table[order(-lambda_table$Median),]
prob <- get_probabilities_df(rank_model, newdata = wc_data_train_GS[,2:3])

wc_data_train_GS <- rbind(wc_data_train_matchday_1)

rm(rank_model)

save(wc_data_train, file = "Africa_Cup/Bayesian_BTD_Ranking/RData/wc_data_train.RData")
save(rank, file = "Africa_Cup/Bayesian_BTD_Ranking/RData/rank_bradley_terry.RData")
