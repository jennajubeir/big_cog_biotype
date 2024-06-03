setwd('/Users/jjubeir/Dropbox (PanLab)/BIG Trial/data/Collaborative Paper/data_JJ_Working')
library(ggplot2)
library(dplyr)
library(scales)
#code for plots used in main text
merged_all_wide = read.csv("merged_all_final_biotype_wide.csv")
merged_all_wide$NoGo_Reaction_Time.bl <- as.numeric(merged_all_wide$NoGo_Reaction_Time.bl) #ensuring numeric
merged_all_wide$NoGo_Reaction_Time.fu <- as.numeric(merged_all_wide$NoGo_Reaction_Time.fu) #ensuring numeric
merged_all_wide$Go_Reaction_Time.bl <- as.numeric(merged_all_wide$Go_Reaction_Time.bl) #ensuring numeric
merged_all_wide$Go_Reaction_Time.fu <- as.numeric(merged_all_wide$Go_Reaction_Time.fu) #ensuring numeric

merged_all_long = read.csv("merged_all_final_biotype_long.csv")
merged_all_long$value <- as.numeric(merged_all_long$value ) #ensuring numeric 

custom_labels <- c("Pre-Tx", "Post-Tx")
custom_labels_hamd <- c("Pre-Treatment", "Week 2", "Post-Treatment")

plot_imaging <- function(data, variable_name, x_label, y_axis, title, y_label,breaks) {
  group_summary <- data[data$variable == variable_name, ] %>%
    group_by(timepoint) %>%
    summarise(mean = mean(value, na.rm = TRUE), sd = sd(value, na.rm = TRUE))
  min_val <- min(merged_all_long$value[merged_all_long$variable == variable_name], na.rm = TRUE)
  max_val <- max(merged_all_long$value[merged_all_long$variable == variable_name], na.rm = TRUE)
  print(min_val)
  print(max_val)
  p <- ggplot() +
    geom_line(data = group_summary, aes(x = timepoint, y = mean), color = "#B75A65", linetype = "solid", size= 1, group = 1) +  # Add line connecting means
    labs("") +
    xlab(x_label) +
    ylab(y_label) +
    ggtitle(title) + 
    scale_x_discrete(labels = custom_labels) +
    scale_y_continuous(breaks = pretty_breaks(n = breaks)) +
    theme_minimal() +
    theme(legend.position = "none",
          axis.text = element_text(size = 18),  
          axis.title = element_text(size = 18),  
          axis.text.x = element_text(size = 16),  
          axis.text.y = if (y_axis == 0) element_blank() else element_text(size = 16),  
          panel.grid.major.x = element_blank(),
          plot.title = element_text(size = 18, hjust = 0.5))
  return(p)
}

#imaging
plot_A_D <- plot_imaging(merged_all_long, "act_gonogo_nogo_vs_go_291082_Medial_dACC","",1, "Activity", "Standard Deviation Units", 4)
ggsave("Biotype_plot_A_D_lines.png", plot = plot_A_D, width = 4, height = 3, units = "in")

plot_C_L <- plot_imaging(merged_all_long, "ppi_gonogo_nogo_vs_go_013136_Left_dlPFC_291082_Medial_dACC", "", 1, "Connectivity", "Standard Deviation Units",4)
ggsave("Biotype_plot_C_L_lines.png", plot = plot_C_L, width = 4, height = 3, units = "in")


#behavioral plot
plot_VI <- plot_imaging(merged_all_long, "Verbal_Interference_Word", "", 1, "Stroop Performance", "Standard Deviation Units",4)
ggsave("Biotype_plot_VI_lines.png", plot = plot_VI, width = 4, height = 3, units = "in")

plot_RT <- plot_imaging(merged_all_long, "ZNoGo_Reaction_Time", "", 1, "Go/NoGo Performance", "Standard Deviation Units",4)
ggsave("Biotype_plot_RT_lines.png", plot = plot_RT, width = 4, height = 3, units = "in")

#brief a 
plot_BA_I <- plot_imaging(merged_all_long, "brief_a_initiatetotal", "", 1, "Initiate","BRIEF-A Subscore", 4 )
ggsave("Biotype_plot_BA_I_lines.png", plot = plot_BA_I, width = 4, height = 3, units = "in")

plot_BA_P <- plot_imaging(merged_all_long, "brief_a_plantotal", "", 1, "Plan and Organize", "BRIEF-A Subscore",4)
ggsave("Biotype_plot_BA_P_lines.png", plot = plot_BA_P, width = 4, height = 3, units = "in")

plot_BA_T <- plot_imaging(merged_all_long, "brief_a_tasktotal", "", 1, "Task Monitor", "BRIEF-A Subscore", 3)
ggsave("Biotype_plot_BA_T_lines.png", plot = plot_BA_T, width = 4, height = 3, units = "in")

plot_BA_WM <- plot_imaging(merged_all_long, "brief_a_wmtotal", "", 1, "Working Memory", "BRIEF-A Subscore", 3)
ggsave("Biotype_plot_BA_WM_lines.png", plot = plot_BA_WM, width = 4, height = 3, units = "in")



#swls plot
plot_SWLS <- plot_imaging(merged_all_long, "swls_total", "", 1, "Satisfaction with Life", "SWLS Total Score", 4)
ggsave("Biotype_plot_SWLS_lines.png", plot = plot_SWLS, width = 4, height = 3, units = "in")

plot_Q1 <- plot_imaging(merged_all_long, "whoqol_dom1trans", "", 1, "Physical Health", "WHOQOL Subscore Per.", 4)
ggsave("Biotype_plot_Q1_lines.png", plot = plot_Q1, width = 4, height = 3, units = "in")

plot_Q2 <- plot_imaging(merged_all_long, "whoqol_dom2trans", "", 1, "Psychological", "WHOQOL Subscore Per.", 4)
ggsave("Biotype_plot_Q2_lines.png", plot = plot_Q2, width = 4, height = 3, units = "in")

plot_Q3 <- plot_imaging(merged_all_long, "whoqol_dom3trans", "", 1, "Social Relationships", "WHOQOL Subscore Per.", 4)
ggsave("Biotype_plot_Q3_lines.png", plot = plot_Q3, width = 4, height = 3, units = "in")

#
df <- data.frame(
  category = c("Remission", "Response"),
  value = c((11/17)*100, (13/17)*100)
)

remission_plot <- ggplot(df, aes(x = category, y = value, fill = category)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.6), width = 0.6) +
  geom_text(aes(label = paste0(round(value, 1), "%")), position = position_dodge(width = 0.6), vjust = -0.5, size = 8) +
  labs(title = "Treatment Outcomes", x = "", y = "Percentage") +
  scale_y_continuous(limits = c(0, 100)) +
  scale_fill_manual(values = c("Remission" = "#B75A65", "Response" = "#B75A65")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18),  # Increase title font size
    axis.title = element_text(size = 18),  # Increase axis label font size
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 18),
    panel.grid.major.x = element_blank(),  # Remove vertical major grid lines
    panel.grid.minor.x = element_blank(),
    legend.position = "none"  # Remove legend
  )
ggsave("Biotype_remission.png", plot = remission_plot, width = 3.5, height = 3, units = "in")



