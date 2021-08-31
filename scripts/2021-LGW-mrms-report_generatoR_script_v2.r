#' ---
#' title: "MRMS ExploreR QC Report"
#' author: ANPC
#' output: html_document
#' 
#' ---
#'
#'
#' Thank you for using MRMS ExploreR. Your MRMS quality control evaluation report will now be produced.
#'
#' ### 1. Project details
#'
#+ echo=FALSE, message=FALSE, fig.width=10, fig.height=4
print(paste0("Project: ", project_name))
print(paste0("User: ", user_name))
print(paste0("QC type used: ", qc_type))
#'
#'
#' ### 2. Import summary
#+ echo=FALSE, message=FALSE, fig.width=10, fig.height=4
print(paste0("Succesfully imported ", project_name, ": ", nrow(mrms_exploreR_data$data_unprocessed), " samples; ", length(mrms_exploreR_data$feature), " MRMS features"))
print(paste0("There are ", mrms_exploreR_data$data_unprocessed$batch %>% unique() %>% length(), " batches in the dataset"))
print(paste0("There are ", mrms_exploreR_data$data_unprocessed$plateID %>% unique() %>% length(), " plates in the dataset"))
print(paste0("There are a total of ", total_data_points, " data points"))
print(paste0(total_percentage_zero_values, " % of total data points are missing or are a 0 value"))
#'
#'### MRMS heatmap to visualise missing values
#+ echo=FALSE, message=FALSE, fig.width=8, fig.height=5
mrms_heatmap
#'
#' ### 3. Total ion count (TIC) quality control check
#' 
#' The first QC check summed the peak area of every MRMS feature target to produce a total ion count signal for each sample. The check is used to identify those samples that:  
#' 
#' * contained low/no signal. Suggesting a preparation error where low volume of the sample had been added to the well or excessive extraction solvent had been added
#' 
#' 
#' 
#' #### Total ion count QC plot
#' 
#' This plot is the result of summing all the lipid target peak areas to form a single total ion count signal. All samples are plotted including LTR samples.
#' 
#+ echo=FALSE, message=FALSE, fig.width=10, fig.height=4
tic_check_p_nc 
#'
#'
#+ echo=FALSE, message=FALSE, fig.width=10, fig.height=4
print(paste0("Samples were considered outliers if their TIC was outside ", temp_answer_tic_nc, " % of the median "))
print(paste0(nrow(tic_qc_fail), " samples FAILED the TIC QC check  ", nrow(tic_qc_fail_ltr)," of which were QCs.  These have been removed from the dataset."))
print(paste0("The dataset now contains ", nrow(mrms_exploreR_data$data_tic_filtered), " samples"))
#'
#' The following samples were removed from the dataset at this check point
#'  
#+ echo=FALSE, message=FALSE, fig.width=10, fig.height=4 
knitr::kable(tic_qc_fail)
#'
#'
#'
#'
#' ### 4. MRMS feature quality control check
#' The QC checks now switched to the MRMS features. m/z features were filtered out due to a user defined signal intensity and appearance frequency.
#' For example the default threshold is to spot zero values and to only keep features with a signal intensity greater than 0 counts in over 50% of samples
#'
#'
#+ echo=FALSE, message=FALSE, fig.width=10, fig.height=4

if(intensity_threshold_ltr == "LTR" | intensity_threshold_ltr == "PQC"){
  print(paste("An MRMS feature was kept in the dataset if it was present in ", intensity_threshold_percentage, " % of ",  intensity_threshold_ltr, " samples with a signal intensity (peak height) greater than ", intensity_threshold, " counts", sep = ""))
}

# if(intensity_threshold_ltr == "PQC"){
#   print(paste("An MRMS feature was kept in the dataset if it was present in ", intensity_threshold_percentage, " % of ",  intensity_threshold_ltr, " samples with a signal intensity (peak height) greater than ", intensity_threshold, " counts", sep = ""))
# }

if(intensity_threshold_ltr == "samples"){
  print(paste("An MRMS feature was kept in the dataset if it was present in ", intensity_threshold_percentage, " % of study ",  intensity_threshold_ltr, " (not LTR or PQC) with a signal intensity (peak height) greater than ", intensity_threshold, " counts", sep = ""))
}

if(intensity_threshold_ltr == "both"){
  print(paste("An MRMS feature was kept in the dataset if it was present in ", intensity_threshold_percentage, " % of both study samples and QC samples with a signal intensity (peak height) greater than ", intensity_threshold, " counts", sep = ""))
}

paste0("QC check 1: ", nrow(percentage_of_zero_plot_data_nc %>% filter(pass == "pass")), " passed QC check and had < 50% zero values.  ", nrow(percentage_of_zero_plot_data_nc %>% filter(pass == "fail")), " failed QC check and had > 50% zero values")
#'
#'
#' ### QC1 visualization
#' 
QC_p_1_nc
#'
#'
#'
#' ### 5. % RSD evaluation in replicate analysis of a QC sample
#' 
#' 
#+ echo=FALSE, message=FALSE, fig.width=10, fig.height=4

print(paste(nrow(rsd_plot_data_nc)-length(which(rsd_plot_data_nc$rsd_loop_rsd < 30)), " lipid targets had a LTR RSD of > 30%", sep=""))
print(paste("Total number of lipid target response ratios with with an LTR RSD of <30% =", length(which(rsd_plot_data_nc$rsd_loop_rsd < 30))))
print(paste("Total number of lipid target response ratios with with an LTR RSD of <20% =", length(which(rsd_plot_data_nc$rsd_loop_rsd < 20))))
print(paste("Total number of lipid target response ratios with with an LTR RSD of <15% =", length(which(rsd_plot_data_nc$rsd_loop_rsd < 15))))
print(paste("Total number of lipid target response ratios with with an LTR RSD of <10% =", length(which(rsd_plot_data_nc$rsd_loop_rsd < 10))))

#'
#'
#' ### QC2 visualization
#'
#'
QC_p_2_nc
#'
#'
#'
#' ### 6. PCA plot to visualize final dataset variance (LTR and samples)
#' 
#' 
#' 
#+ echo=FALSE, message=FALSE, fig.width=10, fig.height=4

print(paste("For creating the QC PCA plots Pareto scaling was used"))
print(paste("PCA plot created using ",  mrms_exploreR_data$data_tic_filtered_qc_filtered %>% select(contains("x")) %>% ncol(), "MRMS features in the final dataset" ))
QC_PCA_1_nc
#'
#'
QC_PCA_2_nc
#'
#'
#' ### 7. Data correction for signal drift
#' 
#' Using the selected replicate QC (PQC or LTR) samples the signals was corrected for signal drift across the run
#'  
#+ echo=FALSE, message=FALSE, fig.width=10, fig.height=4

if(signal_drift_method == "loess"){
print(paste("For correction of the signal drift a loess method (QC-RLSC) was employed using the statTarget package (bioconductor)"))
}

if(signal_drift_method == "RF"){
  print(paste("For correction of the signal drift a Random Forest method (QC-RFSC) was employed using the statTarget package (bioconductor"))
}



print(paste(nrow(rsd_plot_data)-length(which(rsd_plot_data$rsd_loop_rsd < 30)), " lipid targets had a LTR RSD of > 30%", sep=""))
print(paste("Total number of lipid target response ratios with with an LTR RSD of <30% =", length(which(rsd_plot_data$rsd_loop_rsd < 30))))
print(paste("Total number of lipid target response ratios with with an LTR RSD of <20% =", length(which(rsd_plot_data$rsd_loop_rsd < 20))))
print(paste("Total number of lipid target response ratios with with an LTR RSD of <15% =", length(which(rsd_plot_data$rsd_loop_rsd < 15))))
print(paste("Total number of lipid target response ratios with with an LTR RSD of <10% =", length(which(rsd_plot_data$rsd_loop_rsd < 10))))



#'
#'
#'
#' ### Corrected QC visualization
#'
#'
QC_p_2_c
#'
#'
#' ### 8. PCA plot to visualize final corrected dataset variance (replicate QC and samples)
#' 
#' 
#' 
#+ echo=FALSE, message=FALSE, fig.width=10, fig.height=4

print(paste("For creating the QC PCA plots Pareto scaling was used"))
print(paste("PCA plot created using ",  mrms_exploreR_data$corrected_data_tic_filtered_qc_filtered %>% select(contains("x")) %>% ncol(), "MRMS features in the final dataset" ))
QC_PCA_1_c

#'
#'
#'
QC_PCA_2_c



