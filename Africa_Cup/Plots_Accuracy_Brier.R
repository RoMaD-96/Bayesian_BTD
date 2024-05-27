#   ____________________________________________________________________________
#   Libraries                                                               ####

packages <- c(
  "ggplot2",
  "ggpubr",
  "readr",
  "dplyr",
  "tidyr")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))




#   ____________________________________________________________________________
#   Load Data                                                               ####

##  ............................................................................
##  FIFA Ranking                                                            ####

## Statistical Models

load("Africa_Cup/Fifa_Ranking/RData/Accuracy_GS_KS.RData")
FIFA_GS_acc <- round(c(acc_models_GS_KS$GroupStage), 3)
FIFA_KS_acc <- round(c(acc_models_GS_KS$knockoutStage), 3)

load("Africa_Cup/Fifa_Ranking/RData/Brier_GS_KS.RData")
FIFA_GS_brier <- round(unlist(tab_brier_GS_KS$`Group Stage`), 3)
FIFA_KS_brier <- round(unlist(tab_brier_GS_KS$`knockout Stage`), 3)

## ML Algorithms

load("Africa_Cup/Fifa_Ranking/RData/ML_pred_GS.RData")
FIFA_GS_acc_ML <- round(unlist(ML_pred_GS$accuracy[-2]), 3)
FIFA_GS_brier_ML <- round(as.vector(unlist(ML_pred_GS[c(1,3,4)])), 3)

load("Africa_Cup/Fifa_Ranking/RData/ML_pred_KS.RData")
FIFA_KS_acc_ML <- round(unlist(ML_pred_KS$accuracy[-2]), 3)
FIFA_KS_brier_ML <- round(as.vector(unlist(ML_pred_KS[c(1,3,4)])), 3)






##  ............................................................................
##  Unique BT                                                               ####


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Simple BT                                                               ####

## Statistical Models

load("Africa_Cup/Bayesian_BTD_Ranking/RData/Accuracy_GS_KS.RData")
BTD_GS_acc <- round(c(acc_models_GS_KS$GroupStage), 3)
BTD_KS_acc <- round(c(acc_models_GS_KS$knockoutStage), 3)

load("Africa_Cup/Bayesian_BTD_Ranking/RData/Brier_GS_KS.RData")
BTD_GS_brier <- round(unlist(tab_brier_GS_KS$`Group Stage`), 3)
BTD_KS_brier <- round(unlist(tab_brier_GS_KS$`knockout Stage`), 3)

## ML Algorithms

load("Africa_Cup/Bayesian_BTD_Ranking/RData/ML_pred_GS.RData")
BTD_GS_acc_ML <- round(unlist(ML_pred_GS$accuracy[-2]), 3)
BTD_GS_brier_ML <- round(as.vector(unlist(ML_pred_GS[c(1,3,4)])), 3)

load("Africa_Cup/Bayesian_BTD_Ranking/RData/ML_pred_KS.RData")
BTD_KS_acc_ML <- round(unlist(ML_pred_KS$accuracy[-2]), 3)
BTD_KS_brier_ML <- round(as.vector(unlist(ML_pred_KS[c(1,3,4)])), 3)




#   ____________________________________________________________________________
#   Comparison Plots                                                        ####

data_acc <- data.frame(
  model = c("Diag. Infl.", "Biv. Pois.", "Double Pois.", "Random Forest", "MARS", "ANN"),
  GS_BTD = c(BTD_GS_acc,BTD_GS_acc_ML),
  KS_BTD = c(BTD_KS_acc, BTD_KS_acc_ML),
  GS_FIFA = c(FIFA_GS_acc, FIFA_GS_acc_ML),
  KS_FIFA = c(FIFA_KS_acc, FIFA_KS_acc_ML)
)
model_order <- c("Diag. Infl.", "Biv. Pois.", "Double Pois.", "MARS", "ANN", "Random Forest")


data_long <- pivot_longer(
  data_acc,
  cols = -model,
  names_to = "stage",
  values_to = "accuracy"
)

# Split stage into 'Rank' and 'Stage' 
data_long <- data_long %>%
  mutate(Rank = ifelse(grepl("FIFA", stage), "FIFA", "Bayesian BTD"),
         Stage = ifelse(grepl("^GS", stage), "Group Stage", "Knockout Stage")) %>%
  select(-stage)


data_long$model <- factor(data_long$model, levels = model_order)

# Creating the plot
acc_plot <- ggplot(data_long, aes(x = model, y = accuracy, fill = Rank)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("FIFA" = "deepskyblue3", "Bayesian BTD" = "firebrick4"),
                    labels = c("Bayesian BTD", "FIFA")) +
  facet_wrap(~Stage) +
  labs(x = "Model", y = "Accuracy", title = "Accuracy by Model and Stage", fill = "Rank Type") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, hjust = 0.5, size=12),
        axis.text.y = element_text(size=12),
        axis.title = element_text(size=14),
        legend.position = "bottom") +
  coord_flip() +
  geom_text(aes(label = scales::percent(accuracy, accuracy = 1)),
            position = position_dodge(width = 0.9), vjust = 0.5, hjust = 1.2, size = 4, color = "white")

print(acc_plot)


ggsave(filename = "acc_plot_opt_Africa_Cup_MAD.pdf",path = "Plots", plot = acc_plot,
       width = 15, height = 10, device='pdf', dpi=500, useDingbats = FALSE)

##  ............................................................................
##  Brier Score                                                             ####


# Prepare the data frame
data_brier <- data.frame(
  model = c("Diag. Infl.", "Biv. Pois.", "Double Pois.", "Random Forest", "MARS", "ANN"),
  GS_BTD = c(BTD_GS_brier,BTD_GS_brier_ML),
  KS_BTD = c(BTD_KS_brier, BTD_KS_brier_ML),
  GS_FIFA = c(FIFA_GS_brier, FIFA_GS_brier_ML),
  KS_FIFA = c(FIFA_KS_brier, FIFA_KS_brier_ML)
)

# Convert to long format
data_long <- pivot_longer(
  data_brier,
  cols = -model,
  names_to = "stage",
  values_to = "brier_score"
)

# Split stage into 'Rank' and 'Stage' 
data_long <- data_long %>%
  mutate(Rank = ifelse(grepl("FIFA", stage), "FIFA", "Bayesian BTD"),
         Stage = ifelse(grepl("^GS", stage), "Group Stage", "Knockout Stage")) %>%
  select(-stage)


model_order <- c("Diag. Infl.", "Biv. Pois.", "Double Pois.", "MARS", "ANN", "Random Forest")
data_long$model <- factor(data_long$model, levels = model_order)

# Create the plot
brier_plot <- ggplot(data_long, aes(x = model, y = brier_score, fill = Rank)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("FIFA" = "deepskyblue3", "Bayesian BTD" = "firebrick4"),
                    labels = c("Bayesian BTD", "FIFA")) +
  facet_wrap(~Stage) +
  labs(x = "Model", y = "Brier Score", fill = "Ranking System:") +
  theme_bw(base_size = 18) +
  theme(    legend.title = element_text(size = 16, face = "bold"),
            legend.text = element_text(size = 15),
            legend.position = "top",  # Adjust the position of the legend to the right
            text = element_text(size = 15),  # General text size for the plot
            axis.title.x = element_text(size = 20),
            axis.title.y = element_text(size = 20),
            axis.text.y = element_text(size = 18),
            axis.text.x = element_text(size = 18),
            strip.text.x = element_text(size = 18)) +
  coord_flip() +
  geom_text(aes(label = sprintf("%.3f", brier_score)),
            position = position_dodge(width = 0.9), vjust = 0.5, hjust = 1.2, size = 6, color = "white")

print(brier_plot)



ggsave(filename = "brier_plot_opt_Africa_Cup_MAD.pdf",path = "Plots", plot = brier_plot,
       width = 15, height = 10, device='pdf', dpi=500, useDingbats = FALSE)

