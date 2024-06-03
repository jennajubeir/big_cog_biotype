setwd('/Users/jjubeir/Dropbox (PanLab)/BIG Trial/data/Collaborative Paper/data_JJ_Working')
library(dplyr)
library(ggplot2)

merged_all_wide = read.csv("merged_all_final_biotype_wide.csv")
merged_all_wide$per_highmotion_volumes.bl <- as.numeric(gsub("%", "", merged_all_wide$per_highmotion_volumes.bl)) #removing %
merged_all_wide$per_highmotion_volumes.fu <- as.numeric(gsub("%", "", merged_all_wide$per_highmotion_volumes.fu)) #removing %
merged_all_wide$dose_scan.bl <- 0 #ensuring baseline dose set to 0
merged_all_wide$NoGo_Reaction_Time.bl <- as.numeric(merged_all_wide$NoGo_Reaction_Time.bl) #ensuring numeric
merged_all_wide$NoGo_Reaction_Time.fu <- as.numeric(merged_all_wide$NoGo_Reaction_Time.fu) #ensuring numeric

#all sig. primary and secondary outcomes, including confounders of interest (dose, anxiety, high motion)
sig_vars<- c("Verbal_Interference_Word",
                          "hamd_total",
                          "swls_total",
                          "whoqol_dom1trans",
                          "whoqol_dom2trans",
                          "whoqol_dom3trans",
                          "brief_a_initiatetotal",
                          "brief_a_plantotal",
                          "brief_a_tasktotal",
                          "brief_a_wmtotal",
                          "NoGo_Reaction_Time", 
                          "act_gonogo_nogo_vs_go_291082_Medial_dACC",
                          "ppi_gonogo_nogo_vs_go_013136_Left_dlPFC_291082_Medial_dACC", 
             "dass_anxtotal",
             "per_highmotion_volumes", 
             "dose_scan",
)     

#calculating change
for (var in sig_vars) {
  merged_all_wide[[paste0(var, ".change")]] <- merged_all_wide[[paste0(var, ".fu")]] - merged_all_wide[[paste0(var, ".bl")]]
  print(var)
  }

#new df with only variables of interests for correlations
change_vars <- grep(".change", names(merged_all_wide), value = TRUE)
change_vars <- c(change_vars, "med_duration_mri_visit2") #adding med duration
change_df <- merged_all_wide[, change_vars]
change_df$neg_hamd_total.change <- -change_df$hamd_total.change #adding hamd improvement

#checking normality
results <- list()
for (col in colnames(change_df)) {
  shapiro_test <- shapiro.test(change_df[[col]])
  p_value <- round(shapiro_test$p.value, 3)
  results[[col]] <- p_value
}
#show results
print(results)

# spearman correlation matrix for confounders
cor_results_spearman <- rcorr(as.matrix(change_df), type = "spearman")
cor_spearman <- cor_results_spearman$r #correlation coefficients
p_spearman <- cor_results_spearman$P #p-values

# export both matrices 
write.csv(cor_spearman, "cor_matrix_spearman.csv")
write.csv(p_spearman, "p_matrix_spearman.csv")

# pearson correlation matrix for sig. primary and secondary outcomes 
cor_results_pearson <- rcorr(as.matrix(change_df), type = "pearson")
cor_pearson <- cor_results_pearson$r
p_pearson <- cor_results_pearson$P

# export both matrices 
write.csv(cor_pearson, "cor_matrix_pearson.csv")
write.csv(p_pearson, "p_matrix_pearson.csv")


#correlation between dACC activity and HAMD improvement 
correlation <- cor(change_df$act_gonogo_nogo_vs_go_291082_Medial_dACC.change, change_df$neg_hamd_total.change)

#S8 Figure
p <- ggplot(change_df, aes(x = act_gonogo_nogo_vs_go_291082_Medial_dACC.change, y = neg_hamd_total.change)) +
  geom_point(color = "#B75A65") +  
  geom_smooth(method = "lm", se = FALSE, color = "darkgrey", linetype = "solid", alpha = 0.7) +  
  labs(x = "Change in cognitive control circuit activity", y = "Improvement in HDRS-17 Total Score") +  
  theme_minimal() +  
  theme(panel.grid = element_blank(),  
        axis.line = element_line(color = "black"),
        axis.text = element_text(size = 14),  
        axis.title = element_text(size = 14),  
        axis.text.x = element_text(size = 12),  
        axis.text.y = element_text(size = 12)) +  
  annotate("text", x = 1, y = 4, label = paste("r =", round(correlation, 2)), size = 4.5, color = "black")  
ggsave("Correlation_activity_hamd.png", plot = p, width = 6, height = 4, units = "in")


