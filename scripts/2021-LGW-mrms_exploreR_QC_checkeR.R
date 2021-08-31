#set intensity threshold

dlg_message(paste0("Check 1 - intensity values per % of samples filter (filter by flags)"))

# user choice on intensity threshold to be applied
intensity_threshold <- NA

while(is.na(intensity_threshold)) {
  intensity_threshold <- dlgInput("What signal threshold do you want to set for the intensity cut-off filter", "e.g. recommended default = 0")$res %>% as.numeric()
}

# user choice on % of samples that must pass intensity threshold
intensity_threshold_percentage <- NA

while(is.na(intensity_threshold_percentage)) {
  intensity_threshold_percentage <- dlgInput("what % of samples do you want over this threshold?", "e.g. recommended default = 50%")$res %>% as.numeric()
}

# user choice should filter intensity be set for samples/LTR/both
intensity_threshold_ltr <- "blank"

while(intensity_threshold_ltr != "samples" & intensity_threshold_ltr != "LTR" & intensity_threshold_ltr != "PQC" & intensity_threshold_ltr != "all") {
  intensity_threshold_ltr <- dlgInput(paste0("Do you want to apply the filtering using samples/LTR/PQC/all. Recommended default is all"), paste0("samples/LTR/PQC/all"))$res
}

#set up empty listå
failed_metabolites_zero <- NULL

percentage_of_zero_plot_data <- NULL

for(idx_feature in feature){
  filter_data <- mrms_exploreR_data$data_tic_filtered %>% 
    select(type, all_of(idx_feature)) 
  
  if(intensity_threshold_ltr == "LTR"){filter_data <- filter_data %>% filter(type == "LTR")}
  if(intensity_threshold_ltr == "PQC"){filter_data <- filter_data %>% filter(type == "PQC")}
  if(intensity_threshold_ltr == "samples"){filter_data <- filter_data %>% filter(type == "Sample")}
  
  zero_value_index <- which(filter_data[,2] == 0)
  
  percentage_of_zero <- (100/nrow(filter_data))*length(zero_value_index)
  
  if(percentage_of_zero > 50){
    failed_metabolites_zero <- c(failed_metabolites_zero, idx_feature)
  } 
  
  percentage_of_zero_plot_data <- c(percentage_of_zero_plot_data, percentage_of_zero)
}


failed_metabolites <- failed_metabolites_zero
metabolite_list_filtered <- feature[-which(feature %in% failed_metabolites)]

paste0(length(metabolite_list_filtered), " passed the % of zero values QC check.  ", length(failed_metabolites_zero), " failed the QC check.")


#step 2 - %RSD in pool

dlg_message(paste0("Check 2 - % RSD in replicate QCs (LTRs or PQC)"))

intensity_threshold_ltr <- "blank"

while(intensity_threshold_ltr != "PQC" & intensity_threshold_ltr != "LTR" & intensity_threshold_ltr != "none") {
  intensity_threshold_ltr <- dlgInput(paste0("Did you use an LTR/PQC/none."), paste0("LTR/PQC/none"))$res
}

if(intensity_threshold_ltr == "none"){dlg_message(paste0("no QC selected. No RSD filter is possible. Move to next section."))}


if(intensity_threshold_ltr != "none"){

QC_data <- mrms_exploreR_data$data_tic_filtered %>%
  filter(grepl(intensity_threshold_ltr, mrms_exploreR_data$data_tic_filtered$type))

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
  
  rsd_plot_data <- c(rsd_plot_data, rsd_loop_rsd)
  
  if(rsd_loop_rsd > 30 | is.nan(rsd_loop_rsd)){
    failed_metabolites <- c(failed_metabolites, idx_feature)
  }
  
}

}

failed_metabolites_rsd <- failed_metabolites[-which(failed_metabolites %in% failed_metabolites_zero)]

metabolite_list_filtered <- feature[-which(feature %in% failed_metabolites)]

paste0(length(metabolite_list_filtered), " passed the % RSD values QC check.  ", length(failed_metabolites_rsd), " failed the % RSD QC check.")


