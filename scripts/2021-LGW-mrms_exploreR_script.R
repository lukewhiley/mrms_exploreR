# ######### read in data etc

dlg_message("Please select your project folder", type = 'ok'); project_dir <- rstudioapi::selectDirectory() # save project directory root location
setwd(project_dir) # switch the project directory

# set list to store data throughout lipid_exploreR
mrms_exploreR_data <- list()

# create a new directory to store html widgets
if(!dir.exists(paste(project_dir, paste("/",Sys.Date(), "_html_files", sep=""), sep=""))){
  dir.create(paste(project_dir, paste("/",Sys.Date(), "_html_files", sep=""), sep=""))
} 
project_dir_html <- paste(project_dir, paste("/",Sys.Date(), "_html_files", sep=""), sep="")

#user input here for project name and user initials
temp_answer <- "blank"
if(exists("project_name") == TRUE){temp_answer <- dlgInput(paste("the project name is ", project_name, "is this correct?", sep=" "), "yes/no")$res}
while(temp_answer != "yes"){
project_name <- dlgInput("what is the name of the project?", "example_project")$res
mrms_exploreR_data[["project_name"]] <- project_name
temp_answer <- dlgInput(paste("the project name is ", project_name, "is this correct?", sep=" "), "yes/no")$res
}

temp_answer <- "blank"
if(exists("user_name") == TRUE){temp_answer <- dlgInput(paste("the user is ", user_name, "is this correct?", sep=" "), "yes/no")$res}
while(temp_answer != "yes"){
  user_name <- dlgInput("Insert your initials", "example_initials")$res
  mrms_exploreR_data[["user"]] <- user_name
  temp_answer <- dlgInput(paste("the user is ", user_name, "is this correct?", sep=" "), "yes/no")$res
}

temp_answer <- "blank"
if(exists("qc_type") == TRUE){temp_answer <- dlgInput(paste(qc_type, " were used as a QC - is this correct?", sep=" "), "yes/no")$res}
while(temp_answer != "yes"){
  qc_type <- dlgInput("What type of quality control did you use?", "LTR/PQC/none")$res
  mrms_exploreR_data[["qc_type"]] <- qc_type
  temp_answer <- dlgInput(paste(qc_type, " were used as a QC - is this correct?", sep=" "), "yes/no")$res
}


# read in master data
dlg_message("Read in data - Select the CSV file now")
mrms_exploreR_data[["master_data"]] <- read_csv(file = file.choose(.)) %>% clean_names


mrms_exploreR_data[["data_unprocessed"]] <- mrms_exploreR_data[["master_data"]] %>%
  filter(!is.na(type)) %>%
  rename(sampleID = bucket_label) 

mrms_exploreR_data$sampleID <- mrms_exploreR_data[["data_unprocessed"]]$sampleID %>% unique() # create list of sample IDs
mrms_exploreR_data$feature <- mrms_exploreR_data[["data_unprocessed"]] %>% select(contains("x")) %>% names() # create list of mrms features

project_run_order <- mrms_exploreR_data[["data_unprocessed"]] %>% select(sampleID)
project_run_order$plateID <- rep("NA", nrow(project_run_order))
project_run_order$injection_order <- 1:nrow(project_run_order)
project_run_order$batch <- rep(1:nrow(project_run_order))
project_run_order_html <- htmlTable(project_run_order)

htmltools::save_html(project_run_order_html, file = paste(project_dir_html, "/", project_name, "_", user_name, "_run_order_check.html", sep=""))# save plotly widget
browseURL(paste(project_dir_html, "/", project_name, "_", user_name, "_run_order_check.html", sep="")) #open plotly widget in internet browser

temp_answer <- "blank"
temp_answer_2 <- "blank"
while(temp_answer_2 != "yes"){
while(temp_answer != "yes" & temp_answer != "no"){
  temp_answer <- dlgInput("A worklist has just opened in your browser.  Does this match the run order of your analysis?", "yes/no")$res
}

if(temp_answer == "no"){
  dlg_message("OK. Please upload a worklist template csv file now. It will need 4x columns: sampleID, PlateID, injection_order, batch. A template file has been created in your project directory (run_order_template.csv)", type = 'ok')
  temp_tibble <- project_run_order
  temp_tibble$injection_order <- NA
  temp_tibble$plateID <- "plate_x"
  temp_tibble$batch <- 1
  write_csv(temp_tibble, 
            file = paste(project_dir, "/", Sys.Date(), "_run_order_template.csv", sep=""))
  dlg_message("Select this file now", type = 'ok')
  new_project_run_order <- file.choose(.) %>% read_csv()
  colnames(new_project_run_order) <- c("sampleID", "plateID", "injection_order", "batch")
}

mrms_exploreR_data[["data_unprocessed"]]$run_order <- NA
for(idx_ro in 1:nrow(new_project_run_order)){
  #browser()
  #add run order value from worklist template to mrms_exploreR_data[["data_unprocessed"]] 
  mrms_exploreR_data[["data_unprocessed"]]$run_order[grep(new_project_run_order$sampleID[idx_ro], mrms_exploreR_data[["data_unprocessed"]]$sampleID)] <- new_project_run_order$injection_order[idx_ro]
  #add plate number order value from worklist template to mrms_exploreR_data[["data_unprocessed"]] 
  mrms_exploreR_data[["data_unprocessed"]]$plateID[grep(new_project_run_order$sampleID[idx_ro], mrms_exploreR_data[["data_unprocessed"]]$sampleID)] <- new_project_run_order$plateID[idx_ro]
  mrms_exploreR_data[["data_unprocessed"]]$batch[grep(new_project_run_order$sampleID[idx_ro], mrms_exploreR_data[["data_unprocessed"]]$sampleID)] <- new_project_run_order$batch[idx_ro]
}

mrms_exploreR_data[["data_unprocessed"]] <- mrms_exploreR_data[["data_unprocessed"]] %>% arrange(run_order)

new_project_run_order <- mrms_exploreR_data[["data_unprocessed"]] %>% select(sampleID, plateID, run_order, batch)
colnames(new_project_run_order) <- c("sampleID", "plateID", "injection_order", "batch")
new_project_run_order <- new_project_run_order %>% filter(!is.na(injection_order))
new_project_run_order_html <- htmlTable(new_project_run_order)

htmltools::save_html(new_project_run_order_html, file = paste(project_dir_html, "/", project_name, "_", user_name, "_run_order_check.html", sep=""))# save plotly widget
browseURL(paste(project_dir_html, "/", project_name, "_", user_name, "_run_order_check.html", sep="")) #open plotly widget in internet browser

temp_answer_2 <- dlgInput("A new worklist order has just opened in your browser.  Does this match the run order of your analysis?", "yes/no")$res
if(temp_answer_2 == "no"){
  temp_answer <- "no"
}
}

mrms_exploreR_data[["data_unprocessed"]] <- mrms_exploreR_data[["data_unprocessed"]] %>% add_column(mrms_exploreR_data[["data_unprocessed"]]$run_order, .before = 3, .name_repair = "minimal") %>% select(-run_order)
colnames(mrms_exploreR_data[["data_unprocessed"]])[3] <- "run_order"


mrms_exploreR_data[["data_unprocessed"]] <- mrms_exploreR_data[["data_unprocessed"]] %>% filter(!grepl("conditioning", sampleID))

new_project_run_order <- new_project_run_order %>% filter(!grepl("conditioning", sampleID))
plateID <- mrms_exploreR_data[["data_unprocessed"]]$plateID
run_order <- mrms_exploreR_data[["data_unprocessed"]]$run_order

#convert all feature intensity values to numeric
mrms_exploreR_data[["data_unprocessed"]][which(names(mrms_exploreR_data[["data_unprocessed"]]) %in% mrms_exploreR_data$feature)] <- sapply(mrms_exploreR_data[["data_unprocessed"]][which(names(mrms_exploreR_data[["data_unprocessed"]]) %in% mrms_exploreR_data$feature)], as.numeric) %>% as_tibble

#calculate total zero values
total_data_points <- length(mrms_exploreR_data$feature) * length(mrms_exploreR_data$sampleID)
total_zero_values <- mrms_exploreR_data$data_unprocessed %>% 
  select(all_of(mrms_exploreR_data$feature)) 
total_zero_values <- which(total_zero_values == 0) %>% length()

total_percentage_zero_values <- ((100/total_data_points) * total_zero_values) %>% round(2)

mrms_heatmap_data <- mrms_exploreR_data$data_unprocessed %>% select(contains("x")) %>% as.matrix %>% log() %>% t()
mrms_heatmap_data[is.infinite(mrms_heatmap)] <- 0


#create a list of axis settings for plot_ly
x_axis_settings <- list(
  zeroline = FALSE,
  showline = TRUE,
  linecolor = toRGB("black"),
  linewidth = 2,
  showgrid = FALSE,
  title = "Sample index"
)

y_axis_settings <- list(
  zeroline = FALSE,
  showline = TRUE,
  linecolor = toRGB("black"),
  linewidth = 2,
  showgrid = TRUE,
  title = "Feature index"
)

mrms_heatmap <- plot_ly(z = mrms_heatmap_data, type = "heatmap") %>%
  layout(xaxis = x_axis_settings,
         yaxis = y_axis_settings)


