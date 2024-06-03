setwd('/Users/jjubeir/Dropbox (PanLab)/BIG Trial/data/Collaborative Paper/data_JJ_Working')
library(rstatix)
library(dplyr)
library(moments)
library(rcompanion)
library(lsr)

merged_all_wide = read.csv("merged_all_final_biotype_wide.csv")
merged_all_wide$NoGo_Reaction_Time.bl <- as.numeric(merged_all_wide$NoGo_Reaction_Time.bl) #ensuring numeric
merged_all_wide$NoGo_Reaction_Time.fu <- as.numeric(merged_all_wide$NoGo_Reaction_Time.fu) #ensuring numeric
merged_all_wide$Go_Reaction_Time.bl <- as.numeric(merged_all_wide$Go_Reaction_Time.bl) #ensuring numeric
merged_all_wide$Go_Reaction_Time.fu <- as.numeric(merged_all_wide$Go_Reaction_Time.fu) #ensuring numeric

merged_all_long = read.csv("merged_all_final_biotype_long.csv")
merged_all_long$value <- as.numeric(merged_all_long$value) #ensuring numeric 

#remission
remitted <- sum(merged_all_wide$hamd_total.fu <= 7, na.rm = TRUE)
#response
merged_all_wide$reduction_hamd <- (merged_all_wide$hamd_total.bl - merged_all_wide$hamd_total.fu) / merged_all_wide$hamd_total.bl * 100
response_count <- sum(merged_all_wide$reduction_hamd >= 50, na.rm = TRUE)

#primary and secondary outcomes
variables <- c("Maze",
               "Sustained_Attention_Test",
               "Digit_Span_Forward",
               "Switching_of_Attention",
               "Verbal_Interference_Word",
               "Verbal_Interference_Color",
               "hamd_total",
               "swls_total",
               "whoqol_dom1trans",
               "whoqol_dom2trans",
               "whoqol_dom3trans",
               "whoqol_dom4trans",
               "brief_a_initiatetotal",
               "brief_a_plantotal",
               "brief_a_tasktotal",
               "brief_a_wmtotal",
               "Commission_Errors", 
               "Omission_Errors", 
               "NoGo_Reaction_Time",
               "Go_Reaction_Time", 
               "cssrs_highest",
               "act_gonogo_nogo_vs_go_013136_Left_dlPFC",
               "act_gonogo_nogo_vs_go_533294_Right_dlPFC",
               "act_gonogo_nogo_vs_go_291082_Medial_dACC",
               "ppi_gonogo_nogo_vs_go_013136_Left_dlPFC_291082_Medial_dACC",
               "ppi_gonogo_nogo_vs_go_533294_Right_dlPFC_291082_Medial_dACC"
               
)                                             
#function to calculate effect size 
calculate_es <- function(var_name, t){
  temp_data <- merged_all_long %>%
    filter(variable == var_name & timepoint %in% c('bl', 'fu')) %>%
    select(participant_id, timepoint, value) %>%
    rename(var_value = value) %>%
    arrange(participant_id, timepoint) %>%
    mutate(var_value = as.numeric(var_value))
  
  if (t == 'w') {
    return(wilcox_effsize(var_value ~ timepoint, data = temp_data, paired = TRUE)$effsize)
  } else if (t == 't') {
    return(cohensD(var_value ~ timepoint, data = temp_data, method = 'paired'))
  } else {
    stop("Invalid test type. Use 'w' for Wilcoxon or 't' for Cohen's d.")
  }
}
# calculating statistics function for pre/post tx 
calculate_stats <- function(df, var_name) {
  var_name_bl <- paste0(var_name, ".bl")
  var_name_fu <- paste0(var_name, ".fu")
  var <- df[[var_name_bl]]
  var_fu <- df[[var_name_fu]]
  mean_val <- mean(var, na.rm = TRUE)
  median_val <- median(var, na.rm = TRUE)
  sd_val <- sd(var, na.rm = TRUE)
  normal_val <- shapiro.test(var)$p.value
  normal_val_bin <- ifelse(normal_val <= 0.05, 1, 0)
  
  var_fu <- df[[var_name_fu]]
  t_test_p <- ifelse(normal_val_bin==1, wilcox.test(var, var_fu, paired=TRUE)$p.value, t.test(var, var_fu, paired=TRUE)$p.value)
  t_test_t <- ifelse(normal_val_bin==1, wilcox.test(var, var_fu, paired=TRUE)$statistic, t.test(var, var_fu, paired=TRUE)$statistic)
  skewness_hold <- skewness(var)
  es <- ifelse(normal_val_bin==1,calculate_es(var_name,'w'), calculate_es(var_name,'t'))
  data.frame(variable = var_name,
             mean = mean_val,
             median = median_val,
             sd = sd_val,
             normal = normal_val,
             normal_bin = normal_val_bin,
             t_test_pvalue = t_test_p,
             t_test_stat = t_test_t,
             skewness = skewness_hold, 
             effect_size = es)
}

# results df
results <- data.frame(variable = character(),
                      mean = numeric(),
                      median = numeric(),
                      sd = numeric(),
                      normality = numeric(),
                      normality = numeric(),
                      t_test_pvalue = numeric(),
                      t_test_stat = numeric(),
                      skewness = numeric(),
                      effect_size = numeric(),
                      stringsAsFactors = FALSE)
# calculating stats for each primary and secondary outcomes
for (var in variables) {
  stats <- calculate_stats(merged_all_wide, var)
  results <- rbind(results, stats)
}

#exporting results
write.csv(results, "stats_skewness_biotype_0430.csv", row.names = FALSE)
