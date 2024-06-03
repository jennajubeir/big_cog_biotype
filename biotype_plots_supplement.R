setwd('/Users/jjubeir/Dropbox (PanLab)/BIG Trial/data/Collaborative Paper/data_JJ_Working')
library(ggplot2)
library(dplyr)
library(patchwork)
library(tidyr)
library(Hmisc)
library(grid)
library(cowplot)
library("gridExtra")

#plots in the supplement 
merged_all_wide = read.csv("merged_all_final_biotype_wide.csv")
merged_all_wide$NoGo_Reaction_Time.bl <- as.numeric(merged_all_wide$NoGo_Reaction_Time.bl) #ensuring numeric
merged_all_wide$NoGo_Reaction_Time.fu <- as.numeric(merged_all_wide$NoGo_Reaction_Time.fu) #ensuring numeric
merged_all_wide$Go_Reaction_Time.bl <- as.numeric(merged_all_wide$Go_Reaction_Time.bl) #ensuring numeric
merged_all_wide$Go_Reaction_Time.fu <- as.numeric(merged_all_wide$Go_Reaction_Time.fu) #ensuring numeric

merged_all_long = read.csv("merged_all_final_biotype_long.csv")
merged_all_long$value <- as.numeric(merged_all_long$value ) #ensuring numeric 


custom_labels <- c("Pre-Treatment", "Post-Treatment")
custom_labels_hamd <- c("Pre-Treatment", "Week 2", "Post-Treatment")
plot_imaging <- function(data, variable_name, x_label, y_axis, title, y_label) {
  group_summary <- data[data$variable == variable_name, ] %>%
    group_by(timepoint) %>%
    summarise(mean = mean(value, na.rm = TRUE), sd = sd(value, na.rm = TRUE))
  min_val <- min(merged_all_long$value[merged_all_long$variable == variable_name], na.rm = TRUE)
  max_val <- max(merged_all_long$value[merged_all_long$variable == variable_name], na.rm = TRUE)
  
  p <- ggplot() +
    geom_line(data = data[data$variable == variable_name, ], 
              aes(x = timepoint, y = value, group = as.factor(participant_id)), color = "#B75A65", alpha=.35, size=.5) +
    geom_errorbar(data = group_summary, aes(x = timepoint, ymin = mean - sd, ymax = mean + sd), 
                  width = 0.2, color = "#B75A65", size=1) +  #add mean and SE bars
    geom_line(data = group_summary, aes(x = timepoint, y = mean), color = "#B75A65", linetype = "solid", size= 1, group = 1) +  #add line connecting means
    geom_point(data = data[data$variable == variable_name, ], 
               aes(x = timepoint, y = value, group = as.factor(participant_id)), color = "#B75A65", alpha=.5) +  #add points to individual lines
    labs("") +
    xlab(x_label) +
    ylab(y_label) +
    ggtitle(title) + 
    scale_x_discrete(labels = custom_labels) +
    scale_y_continuous(breaks = pretty(range(min_val, max_val), n = 4)) +
    theme_minimal() +
    theme(legend.position = "none",
          axis.text = element_text(size = 14),  
          axis.title = element_text(size = 14),  
          axis.text.x = element_text(size = 12),  
          axis.text.y = if (y_axis == 0) element_blank() else element_text(size = 14),  
          panel.grid.major.x = element_blank(),
          plot.title = element_text(size = 16, hjust = 0.5))  
  return(p)
}

plot_imaging_hamd <- function(data, variable_name, x_label, y_axis, title, y_label) {
  group_summary <- data[data$variable == variable_name, ] %>%
    group_by(timepoint) %>%
    summarise(mean = mean(value, na.rm = TRUE), sd = sd(value, na.rm = TRUE))
  
  min_val <- min(merged_all_long$value[merged_all_long$variable == variable_name], na.rm = TRUE)
  max_val <- max(merged_all_long$value[merged_all_long$variable == variable_name], na.rm = TRUE)
  print(min_val)
  print(max_val)
  
  p <- ggplot() +
    geom_line(data = data[data$variable == variable_name, ], 
              aes(x = factor(timepoint, levels = c("bl", "w2", "fu")), y = value, group = as.factor(participant_id)), color = "#B75A65", alpha=.35, size=.5) +
    geom_errorbar(data = group_summary, aes(x = factor(timepoint, levels = c("bl", "w2", "fu")), ymin = mean - sd, ymax = mean + sd), 
                  width = 0.2, color = "#B75A65", size=1) +  #add mean and SE bars
    geom_line(data = group_summary, aes(x = factor(timepoint, levels = c("bl", "w2", "fu")), y = mean), color = "#B75A65", linetype = "solid", size= 1, group = 1) +  #add line connecting means
    geom_point(data = data[data$variable == variable_name, ], 
               aes(x = factor(timepoint, levels = c("bl", "w2", "fu")), y = value, group = as.factor(participant_id)), color = "#B75A65", alpha=.5) +  #add points to individual lines
    labs("") +
    xlab(x_label) +
    ylab(y_label) +
    scale_x_discrete(labels = custom_labels_hamd) +
    scale_y_continuous(breaks = pretty(range(min_val, max_val), n = 4)) +
    theme_minimal() +
    ggtitle(title) + 
    theme(legend.position = "none",
          axis.text = element_text(size = 14),  
          axis.title = element_text(size = 14),  
          axis.text.x = element_text(size = 12),  
          axis.text.y = if (y_axis == 0) element_blank() else element_text(size = 14),  
          panel.grid.major.x = element_blank(),
          plot.title = element_text(size = 16, hjust = 0.5)) 
  return(p)
}


#imaging
plot_A_L<- plot_imaging(merged_all_long, "act_gonogo_nogo_vs_go_013136_Left_dlPFC", "", 1, "L dLPFC Activity", "Standard Deviation Units")
ggsave("Biotype_plot_A_L.png", plot = plot_A_L, width = 4 ,height = 3, units = "in")

plot_A_R <- plot_imaging(merged_all_long, "act_gonogo_nogo_vs_go_533294_Right_dlPFC","", 1, "R dLPFC Activity", "Standard Deviation Units")
ggsave("Biotype_plot_A_R.png", plot = plot_A_R, width = 4, height = 3, units = "in")

plot_A_D <- plot_imaging(merged_all_long, "act_gonogo_nogo_vs_go_291082_Medial_dACC","",1, "Activity", "Standard Deviation Units")
ggsave("Biotype_plot_A_D.png", plot = plot_A_D, width = 4, height = 3, units = "in")

plot_C_L <- plot_imaging(merged_all_long, "ppi_gonogo_nogo_vs_go_013136_Left_dlPFC_291082_Medial_dACC", "", 1, "Connectivity", "Standard Deviation Units")
ggsave("Biotype_plot_C_L.png", plot = plot_C_L, width = 4, height = 3, units = "in")

plot_C_R <- plot_imaging(merged_all_long, "ppi_gonogo_nogo_vs_go_533294_Right_dlPFC_291082_Medial_dACC","",1, "R dLPFC - dACC Connectivity", "Standard Deviation Units")
ggsave("Biotype_plot_C_R.png", plot = plot_C_R, width = 4, height = 3, units = "in")


#behavioral plot
plot_VI <- plot_imaging(merged_all_long, "Verbal_Interference_Word", "", 1, "Stroop Performance", "Standard Deviation Units")
ggsave("Biotype_plot_VI.png", plot = plot_VI, width = 4, height = 3, units = "in")

plot_RT <- plot_imaging(merged_all_long, "ZNoGo_Reaction_Time", "", 1, "Go/NoGo Performance", "Standard Deviation Units")
ggsave("Biotype_plot_RT.png", plot = plot_RT, width = 4, height = 3, units = "in")

#brief a 

plot_BA_I <- plot_imaging(merged_all_long, "brief_a_initiatetotal", "", 1, "Initiate","BRIEF-A Subscore")
ggsave("Biotype_plot_BA_I.png", plot = plot_BA_I, width = 4, height = 3, units = "in")

plot_BA_P <- plot_imaging(merged_all_long, "brief_a_plantotal", "", 1, "Plan and Organize", "BRIEF-A Subscore")
ggsave("Biotype_plot_BA_P.png", plot = plot_BA_P, width = 4, height = 3, units = "in")

plot_BA_T <- plot_imaging(merged_all_long, "brief_a_tasktotal", "", 1, "Task Monitor", "BRIEF-A Subscore")
ggsave("Biotype_plot_BA_T.png", plot = plot_BA_T, width = 4, height = 3, units = "in")


#swls plot
plot_SWLS <- plot_imaging(merged_all_long, "swls_total", "", 1, "Satisfaction with Life", "SWLS Total Score")
ggsave("Biotype_plot_SWLS.png", plot = plot_SWLS, width = 4, height = 3, units = "in")

plot_Q1 <- plot_imaging(merged_all_long, "whoqol_dom1trans", "", 1, "Physical Health", "WHOQOL Subscore Percentile")
ggsave("Biotype_plot_Q1.png", plot = plot_Q1, width = 4, height = 3, units = "in")

plot_Q2 <- plot_imaging(merged_all_long, "whoqol_dom2trans", "", 1, "Psychological", "WHOQOL Subscore Percentile")
ggsave("Biotype_plot_Q2.png", plot = plot_Q2, width = 4, height = 3, units = "in")

plot_Q3 <- plot_imaging(merged_all_long, "whoqol_dom3trans", "", 1, "Social Relationships", "WHOQOL Subscore Percentile")
ggsave("Biotype_plot_Q3.png", plot = plot_Q3, width = 4, height = 3, units = "in")





