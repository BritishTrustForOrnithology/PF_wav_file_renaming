#' Apply the name to fix existing data
#' Written by Simon Gillings, BTO
#' 13 March 2024
#' 
#' Purpose:
#'    Use the naming info produced by '01 collate naming info for batlogger files.R'
#'    to update the names of wav files (and optionally xml files) in selected folders. 
#'    Also if a folder of Acoustic Pipeline results csv files is found, to update 
#'    the file names listed in those files to the corrected files also.
#'    
#' Usage:
#'    1. To run the whole script in one go, press Ctrl+Shift+Enter
#'    2. When prompted, select the csv file containing the naming info from the previous step.
#'    3. When prompted, specify how many main folders containing audio you want to process (1, 2 or 3)
#'    4. When prompted, specify if you want to rename XML files (WAV files are done by default)
#'    5. Wait...this step will be slow as it will be scanning thousands of files. Updates will be shown on screen. 
#'       When complete it will save various log files in the top level of the external hard drive 

library(rstudioapi)
library(lubridate)

source('functions and defaults.R')

#read the naming info
file_names <- rstudioapi::selectFile(caption = 'Select the csv file containing the good and bad names...')
names <- read.csv(file_names)

#ask how many paths user wants to process
n_paths <- as.numeric(rstudioapi::showPrompt(title = 'How many folders', message = 'Enter the number of folders you want to process, (e.g. 1, 2 or 3):'))
if(!is.numeric(n_paths) | !n_paths %in% 1:3) stop('Number of folders needs to be a number between 1 and 3')

#select the folders of audio
paths <- c(rep(NA,n_paths))
for(p in 1:n_paths) {
  paths[p] <- rstudioapi::selectDirectory(path = dirname(file_names), caption=paste0('Select a folder containing audio to be processed (',p,'/',n_paths,')...'))
}
cat('About to process the following folders:\n')
for(p in paths) cat(p,'\n')

#should XMLs be renamed?
rename_xmls <- rstudioapi::showQuestion(title = 'Rename XMLs?', message = 'Should XML files be renamed?', ok = 'Yes', cancel = 'No')

#iterate over all folders
for(p in 1:length(paths)) {
  cat('Folder:',paths[p],'\n')

  wavs_bad_names <- list.files(paths[p], pattern = "*.wav", recursive = TRUE, full.names = TRUE)
  xmls_bad_names <- list.files(paths[p], pattern = "*.xml", recursive = TRUE, full.names = TRUE)
  cat('N wav files:', length(wavs_bad_names),'\n')  
  cat('N xml files:', length(xmls_bad_names),'\n')  

  #check if the number of wavs matches the number of entries in the naming database
  if(length(wavs_bad_names) != length(xmls_bad_names)) warning('Number of wavs and xmls does not match\n')
  if(length(wavs_bad_names) != nrow(names)) warning('Number of wavs does not match renaming info (wavs=', length(wavs_bad_names), 'names=',nrow(names),')\n')
  if(length(wavs_bad_names) == nrow(names)) message('Success: Number of wavs matches renaming info\n')

  cat("  FIXING WAVS...\n")
  #for each original wav file, try to rename it
  rename_results_wav <- list()
  for(w in 1:length(wavs_bad_names)) {
    this_wav <- wavs_bad_names[w]
    rename_results_wav[w] <- rename_a_wav_file(this_wav)
  } 
  #unpack and report
  rename_results_wav <- data.frame(original_name = wavs_bad_names, outcome = unlist(rename_results_wav))
  table(rename_results_wav$outcome)
  outfile_wav <- file.path(dirname(paths[p]), paste0(format(Sys.Date(), format = "%Y%m%d"), "_rename_batlogger_wav_files_",basename(paths[p]),".csv"))  
  write.csv(rename_results_wav, outfile_wav, row.names = FALSE)
  
  
  #rename the xmls?
  if(rename_xmls==TRUE) {
    cat("  FIXING XMLS...\n")
    #for each original xml file, try to rename it
    rename_results_xml <- list()
    for(x in 1:length(xmls_bad_names)) {
      this_xml <- xmls_bad_names[x]
      rename_results_xml[x] <- rename_a_xml_file(this_xml)
    }
    #unpack and report
    rename_results_xml <- data.frame(original_name = xmls_bad_names, outcome = unlist(rename_results_xml))
    table(rename_results_xml$outcome)
    outfile_xml <- file.path(dirname(paths[p]), paste0(format(Sys.Date(), format = "%Y%m%d"), "_rename_batlogger_xml_files_",basename(paths[p]),".csv"))  
    write.csv(rename_results_xml, outfile_xml, row.names = FALSE)
  }
  
  
  #fix the CSV results
  #is there a csv folder?
  subfolders <- list.dirs(paths[p], recursive = FALSE)
  path_csv <- subfolders[basename(subfolders)=='CSV']
  if(length(path_csv)==1) {
    cat("  FIXING CSVS...\n")
    #list the csvs in the csv folder
    csvs <- list.files(path = path_csv, pattern = ".csv", full.names = TRUE)
    
    #read each csv and fix it
    csv_rows_fixed <- list()
    for(i in 1:length(csvs)) {
      this_csv <- csvs[i]
      csv_rows_fixed[i] <- fix_a_csv(this_csv)
    } 
    csv_results <- data.frame(csv = csvs, percent_fixed = unlist(csv_rows_fixed))
    outfile_csv_results <- file.path(dirname(paths[p]), paste0(format(Sys.Date(), format = "%Y%m%d"), "_fix_csv_results_",basename(paths[p]),".csv"))  
    write.csv(csv_results, outfile_csv_results, row.names = FALSE)
  }
  
  
}


  
