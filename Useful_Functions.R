acc_stan <-function(stan_diag_inf_biv_pois,
                    stan_bivpois,
                    stan_pois,
                    stage_test) 
  {
    N_prev <- length(stage_test$home_team)
    outcome <- c(rep(NA, length(stage_test$home_team)))
    
    for (i in 1:length(stage_test$home_team)) {
      if (stage_test$home_score[i] > stage_test$away_score[i]) {
        outcome[i] <- 1
      }
      else if (stage_test$home_score[i] < stage_test$away_score[i]) {
        outcome[i] <- 3
      }
      else {
        outcome[i] <- 2
      }
    }
    outcome <- as.factor(outcome)
    stage_test <- as_tibble(cbind(stage_test, outcome))
    stage_test <- stage_test %>% relocate(outcome, .before = tournament)
    
    
    
    
    
    sims_diag_inf_biv_pois <- rstan::extract(stan_diag_inf_biv_pois)
    sims_bivpois <- rstan::extract(stan_bivpois)
    sims_poisson <- rstan::extract(stan_pois)
    
    S <- dim(sims_diag_inf_biv_pois$att_raw)[1]
    prob_q_diag_inf_biv_pois <- matrix(NA, N_prev, 3)
    prob_q_pois <- matrix(NA, N_prev, 3)
    prob_q_bivpois <- matrix(NA, N_prev, 3)
    
  
  for (n in 1:N_prev) {
    prob_q_diag_inf_biv_pois[n, 1] <-
      sum(sims_diag_inf_biv_pois$y_prev[, n, 1] >
            sims_diag_inf_biv_pois$y_prev[, n, 2]) /
      S
    prob_q_diag_inf_biv_pois[n, 2] <-
      sum(sims_diag_inf_biv_pois$y_prev[, n, 1] ==
            sims_diag_inf_biv_pois$y_prev[, n, 2]) /
      S
    prob_q_diag_inf_biv_pois[n, 3] <-
      sum(sims_diag_inf_biv_pois$y_prev[, n, 1] <
            sims_diag_inf_biv_pois$y_prev[, n, 2]) /
      S
    prob_q_bivpois[n, 1] <- sum(sims_bivpois$y_prev[, n, 1] >
                                  sims_bivpois$y_prev[, n, 2]) / S
    prob_q_bivpois[n, 2] <- sum(sims_bivpois$y_prev[, n, 1] ==
                                  sims_bivpois$y_prev[, n, 2]) / S
    prob_q_bivpois[n, 3] <- sum(sims_bivpois$y_prev[, n, 1] <
                                  sims_bivpois$y_prev[, n, 2]) / S
    prob_q_pois[n, 1] <- sum(sims_poisson$y_prev[, n, 1] >
                               sims_poisson$y_prev[, n, 2]) / S
    prob_q_pois[n, 2] <- sum(sims_poisson$y_prev[, n, 1] ==
                               sims_poisson$y_prev[, n, 2]) / S
    prob_q_pois[n, 3] <- sum(sims_poisson$y_prev[, n, 1] <
                               sims_poisson$y_prev[, n, 2]) / S
  }
  
  
  results_diag_inf_biv_pois <-
    apply(prob_q_diag_inf_biv_pois, 1, function(x)
      which.max(x))
  results_bivpois <-
    apply(prob_q_bivpois, 1, function(x)
      which.max(x))
  results_pois <- apply(prob_q_pois, 1, function(x)
    which.max(x))
  
  
  
  diag_inf_biv_pois_mat <-
    confusionMatrix(
      factor(results_diag_inf_biv_pois, levels = 1:3),
      factor(stage_test$outcome, levels = 1:3)
    )
  bivpois_mat <-
    confusionMatrix(factor(results_bivpois),
                    factor(stage_test$outcome, levels = 1:3))
  pois_mat <-
    confusionMatrix(factor(results_pois), factor(stage_test$outcome, levels = 1:3))
  
  accuracy <- as.vector(c(
    diag_inf_biv_pois_mat$overall[1],
    bivpois_mat$overall[1],
    pois_mat$overall[1]
  ))
  return(accuracy)
}


brier_score <- function(stan_diag_inf_biv_pois,
                        stan_bivpois,
                        stan_pois,
                        stage_test) 
  {
    N_prev <- length(stage_test$home_team)
    outcome <- c(rep(NA, length(stage_test$home_team)))
    
    for (i in 1:length(stage_test$home_team)) {
      if (stage_test$home_score[i] > stage_test$away_score[i]) {
        outcome[i] <- 1
      }
      else if (stage_test$home_score[i] < stage_test$away_score[i]) {
        outcome[i] <- 3
      }
      else {
        outcome[i] <- 2
      }
    }
    outcome <- as.factor(outcome)
    stage_test <- as_tibble(cbind(stage_test, outcome))
    stage_test <- stage_test %>% relocate(outcome, .before = tournament)
    
    sims_diag_inf_biv_pois <- rstan::extract(stan_diag_inf_biv_pois)
    sims_bivpois <- rstan::extract(stan_bivpois)
    sims_poisson <- rstan::extract(stan_pois)
    
    S <- dim(sims_diag_inf_biv_pois$att_raw)[1]
    prob_q_diag_inf_biv_pois <- matrix(NA, N_prev, 3)
    prob_q_bivpois <- matrix(NA, N_prev, 3)
    prob_q_pois <- matrix(NA, N_prev, 3)
    
    save_p_diag_inf_biv_pois <- c()
    save_p_bivpois <- c()
    save_p_pois <- c()
    
    
    for (n in 1:N_prev) {
      prob_q_diag_inf_biv_pois[n, 1] <-
        sum(sims_diag_inf_biv_pois$y_prev[, n, 1] >
              sims_diag_inf_biv_pois$y_prev[, n, 2]) /
        S
      prob_q_diag_inf_biv_pois[n, 2] <-
        sum(sims_diag_inf_biv_pois$y_prev[, n, 1] ==
              sims_diag_inf_biv_pois$y_prev[, n, 2]) /
        S
      prob_q_diag_inf_biv_pois[n, 3] <-
        sum(sims_diag_inf_biv_pois$y_prev[, n, 1] <
              sims_diag_inf_biv_pois$y_prev[, n, 2]) /
        S
      save_p_diag_inf_biv_pois[n] <-
        prob_q_diag_inf_biv_pois[n, outcome[n]]
      
      prob_q_bivpois[n, 1] <- sum(sims_bivpois$y_prev[, n, 1] >
                                    sims_bivpois$y_prev[, n, 2]) / S
      prob_q_bivpois[n, 2] <- sum(sims_bivpois$y_prev[, n, 1] ==
                                    sims_bivpois$y_prev[, n, 2]) / S
      prob_q_bivpois[n, 3] <- sum(sims_bivpois$y_prev[, n, 1] <
                                    sims_bivpois$y_prev[, n, 2]) / S
      save_p_bivpois[n] <- prob_q_bivpois[n, outcome[n]]
      
      
      prob_q_pois[n, 1] <- sum(sims_poisson$y_prev[, n, 1] >
                                 sims_poisson$y_prev[, n, 2]) / S
      prob_q_pois[n, 2] <- sum(sims_poisson$y_prev[, n, 1] ==
                                 sims_poisson$y_prev[, n, 2]) / S
      prob_q_pois[n, 3] <- sum(sims_poisson$y_prev[, n, 1] <
                                 sims_poisson$y_prev[, n, 2]) / S
      save_p_pois[n] <- prob_q_pois[n, outcome[n]]
      
    }
    
    brier_res <- matrix(0, N_prev, 3)
    for (n in 1:N_prev) {
      brier_res[n, outcome[n]] <- 1
    }
    
    brier_diag_inf_biv_pois <-
      (1 / N_prev) * sum((brier_res - prob_q_diag_inf_biv_pois) ^ 2)
    brier_bivpois <-
      (1 / N_prev) * sum((brier_res - prob_q_bivpois) ^ 2)
    brier_pois <- (1 / N_prev) * sum((brier_res - prob_q_pois) ^ 2)
    
    brier_list <-
      list(brier_diag_inf_biv_pois, brier_bivpois, brier_pois)
    return(brier_list)
  }