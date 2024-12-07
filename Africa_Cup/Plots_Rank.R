#   ____________________________________________________________________________
#   Libraries                                                               ####


# Define packages to use
packages <- c(
  "readr", "dplyr", "tidyr", "bayesplot",
  "caret", "ggplot2", "readxl", "ggflags"
)

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))


#   ____________________________________________________________________________
#   Data                                                                    ####

load("Africa_Cup/Bayesian_BTD_Ranking/RData/rank_bradley_terry.RData")
load("Africa_Cup/Bayesian_BTD_Ranking/RData/ac_data_train.RData")

# Normalize rank$Median
rank$Median <- (rank$Median - median(rank$Median)) / mad(rank$Median)

times <- substr(ac_data_train$date, 1, 6)
times <- as.factor(times)
levels(times) <- 1:length(levels(times))
ac_data_train$date <- as.numeric(as.vector(times))
ac_data_train <- arrange(ac_data_train, date)

### MATCHDAY 1

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

### True Results Match Day 3

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

### QUARTER FINALS

ngames_matchday4 <- 8
ac_data_train_matchday_4 <- data.frame(
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

### SEMIFINALS

ngames_matchday5 <- 4
ac_data_train_matchday_5 <- data.frame(
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

### FINALS

ngames_matchday6 <- 2
ac_data_train_matchday_6 <- data.frame(
  date = rep(length(levels(times)) + 2, ngames_matchday6),
  home_team = c("Nigeria", "Ivory Coast"),
  away_team = c("South Africa", "DR Congo"),
  home_score = c(1, 1),
  away_score = c(1, 0),
  tournament = rep("Africa Cup 2024", ngames_matchday6)
)

### FINAL

ngames_matchday7 <- 2
ac_data_train_matchday_7 <- data.frame(
  date = rep(length(levels(times)) + 2, ngames_matchday7),
  home_team = c("South Africa", "Nigeria"),
  away_team = c("DR Congo", "Ivory Coast"),
  home_score = c(0, 1),
  away_score = c(0, 2),
  tournament = rep("Africa Cup 2024", ngames_matchday7)
)

##  ............................................................................
##  Ranking                                                                 ####

rank_bt <- tibble::rowid_to_column(rank, "Position")

# Combine training data
ac_data_train_ML <- rbind(
  ac_data_train[, -7],
  ac_data_train_matchday_1,
  ac_data_train_matchday_2,
  ac_data_train_matchday_3,
  ac_data_train_matchday_4,
  ac_data_train_matchday_5,
  ac_data_train_matchday_6,
  ac_data_train_matchday_7
)

# Compute outcome
outcome <- vector("integer", length(ac_data_train_ML$home_team))

for (i in seq_along(ac_data_train_ML$home_team)) {
  if (ac_data_train_ML$home_score[i] > ac_data_train_ML$away_score[i]) {
    outcome[i] <- 1
  } else if (ac_data_train_ML$home_score[i] < ac_data_train_ML$away_score[i]) {
    outcome[i] <- 3
  } else {
    outcome[i] <- 2
  }
}

outcome <- as.factor(outcome)
ac_data_train_ML <- cbind(ac_data_train_ML, outcome)
ac_data_train_ML <- ac_data_train_ML %>% relocate(outcome, .before = tournament)

### BT Rank

teams <- unique(ac_data_train_ML$home_team)
team_home <- match(ac_data_train_ML$home_team, teams)
team_away <- match(ac_data_train_ML$away_team, teams)
team1 <- team_home[1:length(ac_data_train_ML$outcome)]
team2 <- team_away[1:length(ac_data_train_ML$outcome)]
ranking1 <- match(ac_data_train_ML$home_team, rank_bt$Parameter)
ranking2 <- match(ac_data_train_ML$away_team, rank_bt$Parameter)

rank1 <- numeric(length(team1))
rank2 <- numeric(length(team2))
for (n in seq_along(team1)) {
  rank1[n] <- rank_bt$Median[ranking1[n]]
  rank2[n] <- rank_bt$Median[ranking2[n]]
}
rank_diff <- rank1 - rank2

rank_type <- rep("BT_Rank", nrow(ac_data_train_ML))

ac_data_plot_BT <- cbind(ac_data_train_ML, rank1, rank2, rank_diff, rank_type)

ac_data_plot_BT <- filter(ac_data_plot_BT, date >= 7)

### FIFA Rank

fifa_ranking_clean <- read_excel("Data/ranking_fifa_dec_2023.xlsx")

# Normalize FIFA rankings
fifa_ranking_clean$ranking <- (fifa_ranking_clean$ranking - median(fifa_ranking_clean$ranking)) / mad(fifa_ranking_clean$ranking)

ranking <- filter(fifa_ranking_clean, team_name %in% rank$Parameter)
rank_fifa <- as.data.frame(ranking)

teams <- unique(ac_data_train_ML$home_team)
team_home <- match(ac_data_train_ML$home_team, teams)
team_away <- match(ac_data_train_ML$away_team, teams)
team1 <- team_home[1:length(ac_data_train_ML$outcome)]
team2 <- team_away[1:length(ac_data_train_ML$outcome)]
ranking1 <- match(ac_data_train_ML$home_team, rank_fifa$team_name)
ranking2 <- match(ac_data_train_ML$away_team, rank_fifa$team_name)

rank1 <- numeric(length(team1))
rank2 <- numeric(length(team2))
for (n in seq_along(team1)) {
  rank1[n] <- rank_fifa$ranking[ranking1[n]]
  rank2[n] <- rank_fifa$ranking[ranking2[n]]
}
rank_diff <- rank1 - rank2

rank_type <- rep("FIFA_Rank", nrow(ac_data_train_ML))

ac_data_plot_FIFA <- cbind(ac_data_train_ML, rank1, rank2, rank_diff, rank_type)

ac_data_plot_FIFA <- filter(ac_data_plot_FIFA, date >= 7)

#   ____________________________________________________________________________
#   Scatterplots                                                            ####

# Combine the ranks datasets

africa_nations <- c(ac_data_train_matchday_1$home_team, ac_data_train_matchday_1$away_team)
country_code <- c(
  "dz", "ao", "bf", "cm", "cv", "cd",
  "eg", "gq", "gm", "gh", "gn", "gw",
  "ci", "ml", "mr", "ma", "mz", "na",
  "ng", "sn", "za", "tz", "tn", "zm"
)

rank_fifa <- rank_fifa %>%
  arrange(team_name) %>%
  filter(team_name %in% africa_nations)

rank_btd <- rank_bt[, c("Parameter", "Median")] %>%
  arrange(Parameter) %>%
  filter(Parameter %in% africa_nations)

rank_bind <- cbind(country_code, rank_fifa, BTD_ranking = rank_btd$Median)

colnames(rank_bind) <- c("Country_Code", "Nations", "FIFA_ranking", "BTD_ranking")

# Calculate correlations
pearson_cor <- cor(rank_bind$FIFA_ranking, rank_bind$BTD_ranking, method = "pearson")
spearman_cor <- cor(rank_bind$FIFA_ranking, rank_bind$BTD_ranking, method = "spearman")
kendall_cor <- cor(rank_bind$FIFA_ranking, rank_bind$BTD_ranking, method = "kendall")

# Create text for the annotations
cor_text <- paste(
  " Pearson: ", round(pearson_cor, 2), "\n",
  " Spearman: ", round(spearman_cor, 2), "\n",
  "Kendall: ", round(kendall_cor, 2)
)

# Create the scatterplot with flags and annotations
ranks_comp <- ggplot(rank_bind, aes(x = BTD_ranking, y = FIFA_ranking)) +
  geom_flag(aes(country = Country_Code), size = 8) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey30", size = 1) +
  labs(
    x = "Normalized Bayesian BTD relative log-strengths",
    y = "Normalized FIFA points"
  ) +
  theme_bw(base_size = 18) +
  theme(
    legend.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 15),
    legend.position = "top",
    text = element_text(size = 15),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 18),
    axis.text.x = element_text(size = 18),
    strip.text.x = element_text(size = 18)
  ) +
  xlim(-0.4, 1.6) +
  ylim(-0.4, 1.6) +
  annotate("text", x = -0.40, y = 1.5, label = cor_text, hjust = 0.25, size = 6, color = "black")

# Print the plot
print(ranks_comp)

ggsave(
  filename = "ranks_comp_Africa_Cup_MAD.pdf",
  path = "Plots",
  plot = ranks_comp,
  width = 12,
  height = 10,
  device = 'pdf',
  dpi = 500,
  useDingbats = FALSE
)

# Combine the datasets for FIFA and BT ranking analysis
ac_data_plot <- rbind(ac_data_plot_FIFA, ac_data_plot_BT)

# Creating the plot with manual color, legend adjustments, and custom facet labels
rank_plot <- ggplot(ac_data_plot, aes(x = rank1, y = rank2, color = factor(date), shape = factor(date))) +
  facet_wrap(~rank_type, labeller = labeller(rank_type = c(BT_Rank = "Bayesian BTD", FIFA_Rank = "FIFA"))) +
  geom_point(size = 4.5, alpha = 0.75) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey30", size = 1) +
  scale_color_manual(
    values = c("7" = "#D55E00", "8" = "#009E73"),
    name = "Stage:",
    labels = c("Group Stage", "Knockout Stage")
  ) +
  scale_shape_manual(
    values = c("7" = 16, "8" = 17),
    name = "Stage:",
    labels = c("Group Stage", "Knockout Stage")
  ) +
  labs(
    x = "Relative Strength Team 1",
    y = "Relative Strength Team 2",
    color = "Stage:",
    shape = "Stage:"
  ) +
  theme_bw(base_size = 18) +
  theme(
    legend.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 15),
    legend.position = "top",
    text = element_text(size = 15),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 18),
    axis.text.x = element_text(size = 18),
    strip.text.x = element_text(size = 18)
  ) +
  scale_x_continuous(limits = c(-0.3, 2)) +
  scale_y_continuous(limits = c(-0.3, 2))

# Print the plot to view it
print(rank_plot)

ggsave(
  filename = "rank_plot_Africa_Cup_MAD.pdf",
  path = "Plots",
  plot = rank_plot,
  width = 15,
  height = 10,
  device = 'pdf',
  dpi = 500,
  useDingbats = FALSE
)


#   ____________________________________________________________________________
#   Box-Plots Rank Difference                                               ####

# Adjusting data with custom facet labels
data <- data.frame(
  RankDiff = c(
    ac_data_plot_FIFA$rank_diff[ac_data_plot_FIFA$date == 7],
    ac_data_plot_BT$rank_diff[ac_data_plot_BT$date == 7],
    ac_data_plot_FIFA$rank_diff[ac_data_plot_FIFA$date == 8],
    ac_data_plot_BT$rank_diff[ac_data_plot_BT$date == 8]
  ),
  Group = rep(
    c("FIFA", "Bayesian BTD", "FIFA", "Bayesian BTD"),
    times = c(
      sum(ac_data_plot_FIFA$date == 7), sum(ac_data_plot_BT$date == 7),
      sum(ac_data_plot_FIFA$date == 8), sum(ac_data_plot_BT$date == 8)
    )
  ),
  Date = factor(
    rep(
      c(7, 7, 8, 8),
      times = c(
        sum(ac_data_plot_FIFA$date == 7), sum(ac_data_plot_BT$date == 7),
        sum(ac_data_plot_FIFA$date == 8), sum(ac_data_plot_BT$date == 8)
      )
    ),
    levels = c(7, 8),
    labels = c("Group Stage", "Knockout Stage")
  )
)

# Plotting with custom colors and faceting by date with renamed facets
box_plot <- ggplot(data, aes(x = Group, y = RankDiff, fill = Group)) +
  geom_boxplot(alpha = 0.85) +
  scale_fill_manual(values = c("FIFA" = "deepskyblue3", "Bayesian BTD" = "firebrick4"), name = "Ranking System:") +
  facet_wrap(~Date) +
  xlab("Ranking System") +
  ylab("Relative Strength Difference") +
  scale_y_continuous(breaks = seq(from = -2, to = ceiling(max(data$RankDiff)), by = 0.5)) +
  theme_bw(base_size = 18) +
  theme(
    legend.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 15),
    legend.position = "top",
    text = element_text(size = 15),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 18),
    axis.text.x = element_text(size = 18),
    strip.text.x = element_text(size = 18)
  ) +
  scale_y_continuous(limits = c(-1.5, 2))

ggsave(
  filename = "box_plot_Africa_Cup_MAD.pdf",
  path = "Plots",
  plot = box_plot,
  width = 15,
  height = 10,
  device = 'pdf',
  dpi = 500,
  useDingbats = FALSE
)

arrange_box_rank <- ggarrange(rank_plot, box_plot, ncol = 1, nrow = 2)

ggsave(
  filename = "arrange_box_rank_Africa_Cup_MAD.pdf",
  path = "Plots",
  plot = arrange_box_rank,
  width = 15,
  height = 15,
  device = 'pdf',
  dpi = 500,
  useDingbats = FALSE
)

ggsave(
  filename = "arrange_box_rank_Africa_Cup_MAD.jpeg",
  path = "Plots",
  plot = arrange_box_rank,
  width = 15,
  height = 15,
  device = 'jpeg',
  dpi = 500
)