#set intensity threshold

dlg_message(paste0("Check 1 - filter by flags: x% of samples must have an intensity value > y"))

# user choice on intensity threshold to be applied
intensity_threshold <- NA

while(is.na(intensity_threshold)) {
  intensity_threshold <- dlgInput("What intensity threshold do you want to set for the y.  Recommended default to evaluate missing values = 0", 0)$res %>% as.numeric()
}

# user choice on % of samples that must pass intensity threshold
intensity_threshold_percentage <- NA

while(is.na(intensity_threshold_percentage)) {
  intensity_threshold_percentage <- dlgInput("what x % of samples do you want over this threshold?  Recommended default = 50 %", 50)$res %>% as.numeric()
}

# user choice should filter intensity be set for samples/LTR/both
intensity_threshold_ltr <- "blank"

while(intensity_threshold_ltr != "keep" & intensity_threshold_ltr != "change") {
  intensity_threshold_ltr <- dlgInput(paste0("You are currently using ", qc_type, " for your QC.  Do you want to keep or change QC type?"), paste0("keep/change"))$res
}

if(intensity_threshold_ltr == "keep"){intensity_threshold_ltr <- "PQC"}

while(intensity_threshold_ltr != "samples" & intensity_threshold_ltr != "LTR" & intensity_threshold_ltr != "PQC" & intensity_threshold_ltr != "all") {
  intensity_threshold_ltr <- dlgInput(paste0("Do you want to apply the filtering using samples/LTR/PQC/all. Recommended default is all"), paste0("samples/LTR/PQC/all"))$res
}

#set up empty listå
failed_metabolites_zero <- NULL

percentage_of_zero_plot_data <- NULL

for(idx_feature in mrms_exploreR_data$feature){
  filter_data <- mrms_exploreR_data$data_for_signal_QC_check %>% 
    select(type, all_of(idx_feature)) 
  
  if(intensity_threshold_ltr == "LTR"){filter_data <- filter_data %>% filter(type == "LTR")}
  if(intensity_threshold_ltr == "PQC"){filter_data <- filter_data %>% filter(type == "PQC")}
  if(intensity_threshold_ltr == "samples"){filter_data <- filter_data %>% filter(type == "Sample")}
  
  zero_value_index <- which(filter_data[,2] == 0)
  
  percentage_of_zero <- (100/nrow(filter_data))*length(zero_value_index)
  
  if(percentage_of_zero > 50){
    failed_metabolites_zero <- c(failed_metabolites_zero, idx_feature)
  } 
  
  percentage_of_zero <- cbind(idx_feature, percentage_of_zero) %>% as_tibble()
  
  percentage_of_zero_plot_data <- bind_rows(percentage_of_zero_plot_data, percentage_of_zero)
}


percentage_of_zero_plot_data$percentage_of_zero <- percentage_of_zero_plot_data$percentage_of_zero %>% as.numeric() %>% round(2)
percentage_of_zero_plot_data <- percentage_of_zero_plot_data %>% arrange(percentage_of_zero)
percentage_of_zero_plot_data$sample_idx <- c(1:nrow(percentage_of_zero_plot_data)) 
percentage_of_zero_plot_data$pass <- "pass"
percentage_of_zero_plot_data$pass[which(percentage_of_zero_plot_data$percentage_of_zero > intensity_threshold_percentage)] <- "fail"
percentage_of_zero_plot_data_pass <- percentage_of_zero_plot_data %>% filter(pass == "pass")
percentage_of_zero_plot_data_fail <- percentage_of_zero_plot_data %>% filter(pass == "fail")

failed_metabolites <- failed_metabolites_zero
metabolite_list_filtered <- mrms_exploreR_data$feature[-which(mrms_exploreR_data$feature %in% failed_metabolites)]

# paste0(length(metabolite_list_filtered), " passed the % of zero values QC check.  ", length(failed_metabolites_zero), " failed the QC check.")

# create visualisation of missing values

#create a list of axis settings for plot_ly
x_axis_settings <- list(
  zeroline = FALSE,
  showline = TRUE,
  linecolor = toRGB("black"),
  linewidth = 2,
  showgrid = FALSE,
  range = c(0, max(percentage_of_zero_plot_data$sample_idx)+10),
  title = "Sample index"
)

y_axis_settings <- list(
  zeroline = FALSE,
  showline = TRUE,
  linecolor = toRGB("black"),
  linewidth = 2,
  showgrid = TRUE,
  range = c(0,110),
  title = "Percentage of zero values"
)

QC_p_1 <- plot_ly(
  type = "scatter", mode = "markers", data = percentage_of_zero_plot_data_pass, x = ~sample_idx, y = ~percentage_of_zero, text = ~idx_feature, color = ~pass, colors = c('#1E90FF', '#FF0000'), 
  marker = list(size = 7, color = '#1E90FF', opacity = 0.5,
                line = list(color = '#000000',width = 1))
) %>% 
  add_trace(type = "scatter", data = percentage_of_zero_plot_data_fail, x = ~sample_idx, y = ~percentage_of_zero, text = ~idx_feature, color = ~pass, 
            marker = list(size = 8, color = '#FF0000')
  ) %>%
  layout(xaxis = x_axis_settings,
         yaxis = y_axis_settings
  ) 


#step 2 - %RSD in pool

dlg_message(paste0("Check 2 - Evaluation of feature variation in replicate QC samples: Cut off is 30 % RSD in replicate analysis of QCs (LTRs or PQC)"))

# intensity_threshold_ltr <- "blank"
# 
# while(intensity_threshold_ltr != "keep" & intensity_threshold_ltr != "change") {
# intensity_threshold_ltr <- dlgInput(paste0("You are currently using ", qc_type, " for your QC. Do you want to keep or change QC type?" ), paste0("keep/change"))$res
# }
# 
# if(intensity_threshold_ltr == "keep"){intensity_threshold_ltr <- "PQC"}
# 
# while(intensity_threshold_ltr != "PQC" & intensity_threshold_ltr != "LTR" & intensity_threshold_ltr != "none") {
#   intensity_threshold_ltr <- dlgInput(paste0("Did you use an LTR/PQC/none."), paste0("LTR/PQC/none"))$res
# }

if(intensity_threshold_ltr != "PQC" & intensity_threshold_ltr != "LTR"){dlg_message(paste0("no QC selected. No RSD filter is possible. Move to next section."))}


if(intensity_threshold_ltr != "none"){

QC_data <- mrms_exploreR_data$data_for_signal_QC_check %>%
  filter(grepl(intensity_threshold_ltr, mrms_exploreR_data$data_for_signal_QC_check$type))

rsd_plot_data <- NULL

for(idx_feature in metabolite_list_filtered){
  rsd_loop_mean <- QC_data %>%
    select(idx_feature) %>%
    as.matrix() %>%
    as.numeric() %>%
    mean(na.rm = TRUE)
  rsd_loop_sd <- QC_data %>%
    select(idx_feature) %>%
    as.matrix() %>%
    as.numeric() %>%
    sd(na.rm = TRUE)
  
  
  rsd_loop_rsd <- (rsd_loop_sd*100)/rsd_loop_mean
  
    if(rsd_loop_rsd > 30 | is.nan(rsd_loop_rsd)){
    failed_metabolites <- c(failed_metabolites, idx_feature)
    }
  
    rsd_loop_rsd <- cbind(idx_feature, rsd_loop_rsd) %>% as_tibble()
    rsd_plot_data <- bind_rows(rsd_plot_data, rsd_loop_rsd)
  
}

}


rsd_plot_data$rsd_loop_rsd <- rsd_plot_data$rsd_loop_rsd %>% as.numeric() %>% round(2)
rsd_plot_data <- rsd_plot_data %>% arrange(rsd_loop_rsd)
rsd_plot_data$sample_idx <- c(1:nrow(rsd_plot_data)) 
rsd_plot_data$pass <- "pass"
rsd_plot_data$pass[which(rsd_plot_data$rsd_loop_rsd > 30)] <- "fail"
rsd_plot_data_pass <- rsd_plot_data %>% filter(pass == "pass")
rsd_plot_data_fail <- rsd_plot_data %>% filter(pass == "fail")

# create visualisation of missing values

#create a list of axis settings for plot_ly
x_axis_settings <- list(
  zeroline = FALSE,
  showline = TRUE,
  linecolor = toRGB("black"),
  linewidth = 2,
  showgrid = FALSE,
  range = c(0, max(rsd_plot_data$sample_idx)+10),
  title = "Sample index"
)

y_axis_settings <- list(
  zeroline = FALSE,
  showline = TRUE,
  linecolor = toRGB("black"),
  linewidth = 2,
  showgrid = TRUE,
  range = c(0,max(rsd_plot_data$rsd_loop_rsd)+10),
  title = paste0("% RSD in ", qc_type, " replicate samples")
)

QC_p_2 <- plot_ly(
  type = "scatter", mode = "markers", data = rsd_plot_data_pass, x = ~sample_idx, y = ~rsd_loop_rsd, text = ~idx_feature, color = ~pass, colors = c('#1E90FF', '#FF0000'), 
  marker = list(size = 7, color = '#1E90FF', opacity = 0.5,
                line = list(color = '#000000',width = 1))
) %>% 
  add_trace(type = "scatter", data = rsd_plot_data_fail, x = ~sample_idx, y = ~rsd_loop_rsd, text = ~idx_feature, color = ~pass, 
            marker = list(size = 8, color = '#FF0000')
  ) %>%
  layout(xaxis = x_axis_settings,
         yaxis = y_axis_settings
  ) 

failed_metabolites_rsd <- failed_metabolites[-which(failed_metabolites %in% failed_metabolites_zero)]

metabolite_list_filtered_2 <- mrms_exploreR_data$feature[-which(mrms_exploreR_data$feature %in% failed_metabolites)]

#paste0(length(metabolite_list_filtered), " feature had an % RSD of < 30% in replicate QC samples.  ", length(failed_metabolites_rsd), " failed the 30 % RSD QC check.")

remove_qc <- "blank"

while(remove_qc != "remove" & remove_qc != "keep") {
  remove_qc <- dlgInput("Do you want to remove the failed features?", "remove/keep")$res
}
