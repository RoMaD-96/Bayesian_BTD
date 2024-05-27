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
  "ggplot2",
  "ggpubr",
  "caret",
  "footBayes",
  "mlr3measures",
  "readxl")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))


#   ____________________________________________________________________________
#   Data                                                                    ####


load("World_Cup/Bayesian_BTD_Ranking/RData/rank_bradley_terry.RData")
load("World_Cup/Bayesian_BTD_Ranking/RData/wc_data_train.RData")

# MAD Normalization
rank$Median <- (rank$Median - median(rank$Median))/mad(rank$Median)


times <- substr(wc_data_train$date, 1, 4)
times <- as.factor(times)
levels(times) <- c(1:length(levels(times)))
wc_data_train$date <- as.numeric(as.vector(times))
wc_data_train <- arrange(wc_data_train, date)



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


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### BT Rank                                                                 ####

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

rank_type <- rep("BT_Rank", nrow(wc_data_train_ML))

wc_data_plot_BT <- cbind(wc_data_train_ML, rank1, rank2, rank_diff, rank_type)

wc_data_plot_BT <- filter(wc_data_plot_BT, date >= 6)


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### FIFA Rank                                                               ####

fifa_ranking <- read.csv2(file = "Data/fifa_ranking.csv", sep = ",", header = TRUE)
fifa_ranking$total_points <- as.numeric(as.vector(fifa_ranking$total_points)) 
fifa_ranking_clean <- fifa_ranking %>% select(country_full, total_points)
fifa_ranking_clean <- fifa_ranking_clean[63707:dim(fifa_ranking)[1], ]
colnames(fifa_ranking_clean) <- c("team_name", "ranking")

# MAD Normalization 
fifa_ranking_clean$ranking <- (fifa_ranking_clean$ranking - median(fifa_ranking_clean$ranking))/mad(fifa_ranking_clean$ranking) 

ranking <- filter(fifa_ranking_clean, fifa_ranking_clean$team_name%in%rank$Parameter )
rank <- as.data.frame(ranking)

teams <- unique(wc_data_train_ML$home_team)
team_home <- match( wc_data_train_ML$home_team, teams)
team_away <- match( wc_data_train_ML$away_team, teams)
team1 <- team_home[1:length(wc_data_train_ML$outcome)]
team2 <- team_away[1:length(wc_data_train_ML$outcome)]
ranking1 <- match(wc_data_train_ML$home_team, rank$team_name)
ranking2 <- match(wc_data_train_ML$away_team, rank$team_name)

rank1 <- rank2 <- c()
for (n in 1:length(team1)){
  rank1[n] <- rank$ranking[ranking1[n]]
  rank2[n] <- rank$ranking[ranking2[n]]
}
rank_diff <- rank1-rank2

rank_type <- rep("FIFA_Rank", nrow(wc_data_train_ML))


wc_data_plot_FIFA <- cbind(wc_data_train_ML,rank1,rank2,rank_diff,rank_type)

wc_data_plot_FIFA <- filter(wc_data_plot_FIFA, date >= 6)

#   ____________________________________________________________________________
#   Scatterplots                                                            ####

wc_data_plot <- rbind(wc_data_plot_FIFA, wc_data_plot_BT)


rank_plot <- ggplot(wc_data_plot, aes(x = rank1, y = rank2, color = factor(date))) +
  facet_wrap(~rank_type, labeller = labeller(rank_type = c(BT_Rank = "Bayesian BTD", FIFA_Rank = "FIFA"))) +
  geom_point(size = 4.5, alpha = 0.75) + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey30", size = 1) +  # Add a bisection line
  scale_color_manual(
    values = c("6" = "#D55E00", "7" = "#009E73"), 
    name = "Stage:",  # Custom legend title
    labels = c("Group Stage", "Knockout Stage")  
  ) +
  labs(x = "Rank 1",
       y = "Rank 2",
       color = "Stage:") +  # Label for the color legend
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
  )


ggsave(filename = "rank_plot_WC_MAD.pdf",path = "Plots", plot = rank_plot,
       width = 15, height = 10, device='pdf', dpi=500, useDingbats = FALSE)


#   ____________________________________________________________________________
#   Box-Plots Rank Difference                                               ####

# Adjusting data with custom facet labels
data <- data.frame(
  RankDiff = c(wc_data_plot_FIFA$rank_diff[wc_data_plot_FIFA$date == 6],
               wc_data_plot_BT$rank_diff[wc_data_plot_BT$date == 6],
               wc_data_plot_FIFA$rank_diff[wc_data_plot_FIFA$date == 7],
               wc_data_plot_BT$rank_diff[wc_data_plot_BT$date == 7]),
  Group = rep(c("FIFA", "Bayesian BTD", "FIFA", "Bayesian BTD"), 
              times = c(sum(wc_data_plot_FIFA$date == 6), sum(wc_data_plot_BT$date == 6), 
                        sum(wc_data_plot_FIFA$date == 7), sum(wc_data_plot_BT$date == 7))),
  Date = factor(rep(c(6, 6, 7, 7), 
                    times = c(sum(wc_data_plot_FIFA$date == 6), sum(wc_data_plot_BT$date == 6), 
                              sum(wc_data_plot_FIFA$date == 7), sum(wc_data_plot_BT$date == 7))),
                levels = c(6, 7),
                labels = c("Group Stage", "Knockout Stage"))
)



# Plotting with custom colors and faceting by date with renamed facets
box_plot <- ggplot(data, aes(x = Group, y = RankDiff, fill = Group)) +
  geom_boxplot(alpha=0.85) +
  scale_fill_manual(values = c("FIFA" = "deepskyblue3", "Bayesian BTD" = "firebrick4"), name = "Ranking System:") +
  facet_wrap(~Date) +  # Faceting by Date with custom names
  xlab("Ranking System") +
  ylab("Rank Difference") +
  scale_y_continuous(breaks = seq(from = -2, to = ceiling(max(data$RankDiff)), by = 0.5)) +
  theme_bw(base_size = 18) +
  theme(
    legend.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 15),
    legend.position = "top",  # Adjust the position of the legend to the top
    text = element_text(size = 15),  # General text size for the plot
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 18),
    axis.text.x = element_text(size = 18),
    strip.text.x = element_text(size = 18)
  )

ggsave(filename = "box_plot_WC_MAD.pdf",path = "Plots", plot = box_plot,
       width = 15, height = 10, device='pdf', dpi=500, useDingbats = FALSE)


arrange_box_rank <- ggarrange(rank_plot, box_plot, ncol = 1, nrow = 2)


ggsave(filename = "arrange_box_rank_WC_MAD.pdf",path = "Plots", plot = arrange_box_rank,
       width = 15, height = 15, device='pdf', dpi=500, useDingbats = FALSE)

ggsave(filename = "arrange_box_rank_WC_MAD.jpeg",path = "Plots", plot = arrange_box_rank,
       width = 15, height = 15, device='jpeg', dpi=500)
