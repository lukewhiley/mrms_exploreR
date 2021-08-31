# signal drift/batch correction
# this script uses the LTRs (pooled QC) to correct the signal drift across the run

library(statTarget)

if(!dir.exists(paste(project_dir, "/", Sys.Date(), "_signal_correction_results", sep=""))){
  dir.create(paste(project_dir, "/", Sys.Date(), "_signal_correction_results", sep=""))
}
setwd(paste(project_dir, "/", Sys.Date(), "_signal_correction_results", sep=""))

#fill infinite values created at the ratio step with a small value
sil_trend <- mrms_exploreR_data$data_for_signal_drift_correction
sil_trend[sapply(sil_trend, is.infinite)] <- 1e-5


#this section creates the required metadata file for statTarget::shiftCor

sil_trend_cor_meta <- sil_trend %>% select(sampleID, batch) 
#sil_trend_cor_meta$batch <- 1
sil_trend_cor_meta$class <- 1
sil_trend_cor_meta$class[grep(paste0(qc_type), sil_trend_cor_meta$sampleID)] <- NA
sil_trend_cor_meta$order <- c(1:nrow(sil_trend_cor_meta))
sil_trend_cor_meta <- as_tibble(sil_trend_cor_meta)

#ensure an LTR is "first" and "last" in the worklist order. Required for statTarget::shiftCor
LTR_locations <- grep(paste0(qc_type), sil_trend_cor_meta$sampleID)


#create first LTR
if(LTR_locations[1] > 1){
sil_trend_cor_meta$order[LTR_locations[1]] <- 1
sil_trend_cor_meta$order[1:(LTR_locations[1]-1)] <- sil_trend_cor_meta$order[1:(LTR_locations[1]-1)] +1 # re-label the samples
}

#create last LTR
if(LTR_locations[length(LTR_locations)] < nrow(sil_trend_cor_meta)){
sil_trend_cor_meta$order[LTR_locations[length(LTR_locations)]] <- nrow(sil_trend_cor_meta)
sil_trend_cor_meta$order[(LTR_locations[length(LTR_locations)]+1):nrow(sil_trend_cor_meta)] <- sil_trend_cor_meta$order[(LTR_locations[length(LTR_locations)]+1):nrow(sil_trend_cor_meta)]-1
}


#create new labels simply QCx and samples
sil_trend_cor_meta$sample <- NA
#QC
sil_trend_cor_meta$sample[is.na(sil_trend_cor_meta$class)] <- paste("QC", 1:length(sil_trend_cor_meta$sample[is.na(sil_trend_cor_meta$class)]), sep="")
#Sample
sil_trend_cor_meta$sample[!is.na(sil_trend_cor_meta$class)] <- paste("sample", 1:length(sil_trend_cor_meta$sample[!is.na(sil_trend_cor_meta$class)]), sep="")

#order
sil_trend_cor_meta <- sil_trend_cor_meta %>% arrange(`order`)
sil_trend_cor_meta_2 <- sil_trend_cor_meta %>% select(sample, batch, class, order)

# write out as csv (requirement for statTarget::shiftCor)
write_csv(x = sil_trend_cor_meta_2,
          file = paste(project_dir, "/", Sys.Date(), "_signal_correction_results", "/sil_trend_cor_meta.csv", sep="")
          )

#create data for statTarget::shiftCor
sil_trend_cor_data <- sil_trend
# colnames(sil_trend_cor_data) <- gsub("[():]", "_", colnames(sil_trend_cor_data))
sil_trend_cor_data <- sil_trend_cor_meta %>% select(sampleID, sample) %>% right_join(sil_trend_cor_data, by = 'sampleID') %>% select(sample, contains("x"))
sil_trend_cor_data_2 <- as_tibble(cbind(nms = names(sil_trend_cor_data), t(sil_trend_cor_data)))
colnames(sil_trend_cor_data_2) <- sil_trend_cor_data_2[1,]
sil_trend_cor_data_2 <- sil_trend_cor_data_2[-1,]
sil_trend_cor_data_2[,2:ncol(sil_trend_cor_data_2)] <- sapply(sil_trend_cor_data_2[,2:ncol(sil_trend_cor_data_2)], as.numeric)
sil_trend_cor_data_2 <- sil_trend_cor_data_2 %>% rename(name=sample)

#replace missing values with a very low value
sil_trend_cor_data_2[sil_trend_cor_data_2==0] <- NA
sil_trend_cor_data_2[is.na(sil_trend_cor_data_2)] <- min(sil_trend_cor_data_2[,-1], na.rm = TRUE)/1000

write_csv(x = sil_trend_cor_data_2, 
          file = paste(project_dir, "/", Sys.Date(), "_signal_correction_results",  "/sil_trend_cor_data.csv", sep="")
          )


samPeno <- paste(project_dir, "/", Sys.Date(), "_signal_correction_results", "/sil_trend_cor_meta.csv", sep="")
samFile <- paste(project_dir, "/", Sys.Date(), "_signal_correction_results",  "/sil_trend_cor_data.csv", sep="")

signal_drift_method <- "blank"
# if(workflow_choice == "default"){
#   signal_drift_method <- "RF"
# }
while(signal_drift_method != "loess"& signal_drift_method != "RF"){
  signal_drift_method <- dlgInput("What method do you wish to use? Loess or random forrest?", "loess/RF")$res
}

if(signal_drift_method == "RF"){
shiftCor(samPeno = samPeno,
         samFile =  samFile,
         Frule = 0.8,
         ntree = 500,
         MLmethod = 'QCRFSC',
         QCspan = 0,
         imputeM = "KNN",
         plot = TRUE,
         coCV = 1000
         )
}

if(signal_drift_method == "loess"){
  shiftCor(samPeno = samPeno,
           samFile =  samFile,
           Frule = 0.8,
           MLmethod = 'QCRLSC',
           QCspan = 0,
           imputeM = "KNN",
           plot = TRUE,
           coCV = 1000
  )
}

corrected_data <- read_csv(paste(project_dir, "/", Sys.Date(), "_signal_correction_results", "/statTarget/shiftCor/After_shiftCor/shift_all_cor.csv", sep=""))
corrected_data <- sil_trend_cor_meta %>% select(sampleID, sample) %>% right_join(corrected_data, by = 'sample') %>% select(-sample, -class) %>% arrange(sampleID)

#add abck in the additional sample data
corrected_data <- mrms_exploreR_data$data_tic_filtered_qc_filtered %>% 
  select(sampleID, run_order, type, plateID) %>% right_join(corrected_data, by = "sampleID")


#corrected_feature <- corrected_data %>% select(contains("x")) %>% names()





