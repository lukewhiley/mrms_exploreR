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
#' The following heatmap shows Log10 intensity of each feature in each sample.
#+ echo=FALSE, message=FALSE, fig.width=8, fig.height=5
mrms_heatmap
#'
#' ### 3. Total ion count (TIC) quality control check
#' 
#' The first QC check summed the feature intensity of every MRMS feature to produce a total ion count signal for each sample. The check is used to identify those samples that:  
#' 
#' * contained low/no total signal. Suggesting a preparation error where low volume of the sample had been added to the well or excessive extraction solvent had been added
#' 
#' 
#' 
#' #### Total ion count QC plot
#' 
#' This plot is the result of summing all the MRMS feature peak areas to form a single total ion count signal. All samples are plotted including LTR samples.
#' 
#+ echo=FALSE, message=FALSE, fig.width=10, fig.height=4
print(paste0("Samples were considered outliers if their TIC was outside ", temp_answer_tic_nc, " % of the median "))
print(paste0(nrow(tic_qc_fail_nc), " samples FAILED the TIC QC check ", nrow(tic_qc_fail_ltr_nc)," of which were ", qc_type ,  ". These were removed from the dataset."))
print(paste0("The dataset now contains ", 
             mrms_exploreR_data$data_tic_filtered %>% filter(type == "Sample") %>% nrow(), 
             " samples and ",
             mrms_exploreR_data$data_tic_filtered %>% filter(type == paste0(qc_type)) %>% nrow(),
             " ",
             paste0(qc_type),
             " samples"
))
                                                             
tic_check_p_nc 
#'
#' ### 4. Individual feature intensity threshold QC check
#' Individual feature QC check 1. Each m/z feature was assessed and were subsequently filtered out using a user defined signal intensity threshold and appearance frequency.
#' For example the default threshold is to spot zero values and to only keep features with a signal intensity greater than 0 counts in over 50% of samples
#'
#'
#+ echo=FALSE, message=FALSE, fig.width=10, fig.height=4

if(intensity_threshold_ltr == "LTR" | intensity_threshold_ltr == "PQC"){
  print(paste("An MRMS feature was kept in the dataset if it was present in ", intensity_threshold_percentage, " % of ",  intensity_threshold_ltr, " samples with a signal intensity (peak height) greater than ", intensity_threshold, " counts", sep = ""))
}


if(intensity_threshold_ltr == "samples"){
  print(paste("An MRMS feature was kept in the dataset if it was present in ", intensity_threshold_percentage, " % of study ",  intensity_threshold_ltr, " (not LTR or PQC) with a signal intensity (peak height) greater than ", intensity_threshold, " counts", sep = ""))
}

if(intensity_threshold_ltr == "all"){
  print(paste("An MRMS feature was kept in the dataset if it was present in ", intensity_threshold_percentage, " % of both study samples and QC samples with a signal intensity (peak height) greater than ", intensity_threshold, " counts", sep = ""))
}

paste0("QC check 1: ", nrow(percentage_of_zero_plot_data_nc %>% filter(pass == "pass")), " passed the QC check.  ", nrow(percentage_of_zero_plot_data_nc %>% filter(pass == "fail")), " failed the QC check.")
print(paste0("The dataset now contains ", nrow(percentage_of_zero_plot_data_nc %>% filter(pass == "pass")), " features"))
#'
#'
#' ### QC1 visualization
#'The below plot displays the % of missing values for each feature. Feature index is ordered by % of missing values.
#+ echo=FALSE, message=FALSE, fig.width=10, fig.height=4
QC_p_1_nc
#'
#'
#'
#' ### 5. % RSD evaluation in replicate analysis of a QC sample
#' Individual feature QC check 2. Each m/z feature that passed QC check 4 was assessed for %RSD in replicate analysis of a QC sample. Features were subsequently filtered if the %RSD was greater than 30%.
#' 
#+ echo=FALSE, message=FALSE, fig.width=10, fig.height=4
print(paste0("The QC check was perfromed on ", nrow(percentage_of_zero_plot_data_nc %>% filter(pass == "pass")), " features which passed QC check 1."))

print(paste(nrow(rsd_plot_data_nc)-length(which(rsd_plot_data_nc$rsd_loop_rsd < 30)), " MRMS features had a LTR RSD of > 30%", sep=""))
print(paste("Total number of MRMS feature with an LTR RSD of <30% =", length(which(rsd_plot_data_nc$rsd_loop_rsd < 30))))
print(paste("Total number of MRMS feature with an LTR RSD of <20% =", length(which(rsd_plot_data_nc$rsd_loop_rsd < 20))))
print(paste("Total number of MRMS feature with an LTR RSD of <15% =", length(which(rsd_plot_data_nc$rsd_loop_rsd < 15))))
print(paste("Total number of MRMS feature with an LTR RSD of <10% =", length(which(rsd_plot_data_nc$rsd_loop_rsd < 10))))

#'
#'
#' ### QC2 visualization
#'The below plot displays the % RSD of each of the remaining features in the replicate QC samples. Feature index is ordered by RSD%.
#+ echo=FALSE, message=FALSE, fig.width=10, fig.height=4
QC_p_2_nc
#'
#'
#'
#' ### 6. PCA to visualize final dataset variance (LTR and samples)
#' PCA created with Pareto scaling
#' 
#' 
#+ echo=FALSE, message=FALSE, fig.width=10, fig.height=4
print(paste0("PCA created using ",  
             mrms_exploreR_data$data_tic_filtered_qc_filtered %>% filter(type == "Sample") %>% nrow(),
             " samples, ",
             mrms_exploreR_data$data_tic_filtered_qc_filtered %>% filter(type == qc_type) %>% nrow(),
             " ",
             qc_type,
             " and ", 
             mrms_exploreR_data$data_tic_filtered_qc_filtered %>% select(contains("x")) %>% ncol(), 
             " MRMS features" ))
print(paste0("coloured by sample/QC"))

#' PCA coloured by Sample type
#+ echo=FALSE, message=FALSE, fig.width=10, fig.height=4
QC_PCA_1_nc
#'
#'
#'
#' PCA coloured by plateID
#+ echo=FALSE, message=FALSE, fig.width=10, fig.height=4
QC_PCA_2_nc
#'
#'
#' ### 7. Data correction for signal drift
#' 
#' Using the selected replicate QC (PQC or LTR) samples the signals was corrected for batch and signal drift across the run.
#'  
#+ echo=FALSE, message=FALSE, fig.width=10, fig.height=4
print(paste0("Signal correction was performed on ",
             mrms_exploreR_data$data_for_signal_QC_check %>% select(contains("x")) %>% ncol(),
             " MRMS features across ",
             mrms_exploreR_data$data_for_signal_QC_check %>% filter(type == "Sample") %>% nrow(),
             " samples, using ", 
             mrms_exploreR_data$data_for_signal_QC_check %>% filter(type == qc_type) %>% nrow(),
             " ",
             paste0(qc_type),
             " samples"
             ))

if(signal_drift_method == "loess"){
print(paste("For correction of the signal drift a loess method (QC-RLSC) was employed using the statTarget package (bioconductor)"))
}

if(signal_drift_method == "RF"){
  print(paste("For correction of the signal drift a Random Forest method (QC-RFSC) was employed using the statTarget package (bioconductor"))
}

#' ### 8. Signal correction results
#' The below plot represents the total ion count of each sample following the signal correction.
#+ echo=FALSE, message=FALSE, fig.width=10, fig.height=4
print(paste0("Samples were considered outliers if their TIC was outside ", temp_answer_tic_c, " % of the median "))
print(paste0(nrow(tic_qc_fail_c), " samples FAILED the TIC QC check, ", nrow(tic_qc_fail_ltr_c)," of which were ", qc_type ,  ". These were removed from the dataset."))
print(paste0("The dataset now contains ", nrow(mrms_exploreR_data$corrected_data_tic_filtered), " samples"))
tic_check_p_c 
#' 
#' Following signal correction a % RSD filter in the replicate QC samples was applied to assess the performance of each feature across the total run
#+ echo=FALSE, message=FALSE, fig.width=10, fig.height=4
print(paste(nrow(rsd_plot_data)-length(which(rsd_plot_data$rsd_loop_rsd < 30)), " MRMS features had a LTR RSD of > 30%", sep=""))
print(paste("Total number of MRMS features with an LTR RSD of <30% =", length(which(rsd_plot_data$rsd_loop_rsd < 30))))
print(paste("Total number of MRMS feature with an LTR RSD of <20% =", length(which(rsd_plot_data$rsd_loop_rsd < 20))))
print(paste("Total number of MRMS feature with an LTR RSD of <15% =", length(which(rsd_plot_data$rsd_loop_rsd < 15))))
print(paste("Total number of MRMS feature with an LTR RSD of <10% =", length(which(rsd_plot_data$rsd_loop_rsd < 10))))
#'
#'
#'
#' ### %RSD QC visualization on signal corrected data
#'
#'The below plot displays the % RSD post-signal correction of each of the remaining features in the replicate QC samples. Feature index is ordered by RSD%.
#'
#+ echo=FALSE, message=FALSE, fig.width=10, fig.height=4
QC_p_2_c
#'
#'
#' ### 9. PCA plot to visualize final corrected dataset variance (replicate QC and samples)
#' PCA created with Pareto scaling
#' 
#' 
#+ echo=FALSE, message=FALSE, fig.width=10, fig.height=4
print(paste0("PCA created using ",  
             mrms_exploreR_data$data_tic_filtered_qc_filtered %>% filter(type == "Sample") %>% nrow(),
             " samples, ",
             mrms_exploreR_data$data_tic_filtered_qc_filtered %>% filter(type == qc_type) %>% nrow(),
             " ",
             qc_type,
             " and ", 
             mrms_exploreR_data$data_tic_filtered_qc_filtered %>% select(contains("x")) %>% ncol(), 
             " MRMS features" ))

#' PCA coloured by Sample type
#+ echo=FALSE, message=FALSE, fig.width=10, fig.height=4
QC_PCA_1_c
#'
#' PCA coloured by plateID
#+ echo=FALSE, message=FALSE, fig.width=10, fig.height=4
QC_PCA_2_c
#'