#' 01 collate naming info for batlogger files
#' Written by Simon Gillings, BTO
#' 13 March 2024
#' 
#' 
#' Purpose:
#'    To scan a folder of the original audio files, look for associated XML files
#'    and use the information therein to propose new names for all wav files 
#'    incorporating recording datetime and location info. It will also list what
#'    the former 'bad' filenames were, these being comprised of a concatenation
#'    of the file location. This information will be saved in a csv file for the 
#'    next step, to be performed by a separate script
#' 
#' Usage:
#'    1. To run the whole script in one go, press Ctrl+Shift+Enter
#'    2. User will be prompted to select the folder where the original audio files are located.
#'    3. User will be prompted to specify a location where the naming info will be save. Recommend 
#'       this is the top level of the external hard drive.
#'    4. The script should gradually iterate over all the sites on the drive and 
#'       at the end it will produce a csv summarising good and bad filenames for 
#'       all wav files. It will also print a summary of the % of files that had
#'       xml information for this purpose.

require(rstudioapi)
require(lubridate)
require(xml2)

source('E:/functions and defaults.R')

#select the original audio path
path_original <- rstudioapi::selectDirectory(caption = 'Select folder containing the original audio...')

# #select the path where the badly renamed audio is located
# path_renamed_bad <- rstudioapi::selectDirectory(path = dirname(path_original))
# 
# #select the folder containing the CSV results files
# path_csv_results <- rstudioapi::selectDirectory(path = path_renamed_bad)

#select where to save the log file
path_log <- rstudioapi::selectDirectory(dirname(path_original), caption = 'Select folder to save log and other diagnostics...')
#file_log <- file.path(path_log, paste0(format(Sys.Date(), format = "%Y%m%d"), "_audit_batlogger_files_naming_log.txt"))
#sink(file = file_log, append = FALSE, type = c("output", "message"), split = FALSE)

#get thelist of sites from the original audio
sites_original <- list.dirs(path_original, recursive = FALSE)
cat('There are', length(sites_original), 'sites to process\n')

#iterate over sites
all_sites_results <- list()
for(s in 1:length(sites_original)) {
  this_site <- sites_original[s]
  print(this_site)
  wavs_original <- list.files(path = this_site, pattern = 'wav', recursive = TRUE, full.names = TRUE)  
  
  #check as some folders don't contain any wavs
  if(length(wavs_original)==0) {
    cat('WARNING: No wavs for site',basename(this_site),'\n')
    next
  }
  
  #assuming there are wavs, report contents then process each file
  cat(' ',basename(this_site), 'contains',length(wavs_original),'wav files to process\n')
  
  #iterate over wavs, proposing rename for each file
  one_site_results <- list()
  for(w in 1:length(wavs_original)) {
    one_site_results[[w]] <- process_a_file(wavs_original[w])
  }
  one_site_results <- do.call(rbind,one_site_results)  
  one_site_results$site_long <- basename(this_site)
  one_site_results$site_short <- gsub("Location ", "", basename(this_site))
  
  #compile this site's results wth all others
  all_sites_results[[s]] <- one_site_results
  
  #tidy up
  rm(this_site)
  rm(wavs_original)
  rm(one_site_results)
}
#unpack
all_sites_results <- do.call(rbind, all_sites_results)


#save the results
file_results <- file.path(path_log, paste0(format(Sys.Date(), format = "%Y%m%d"), "_audit_batlogger_files_good_and_bad_names.csv"))
write.csv(all_sites_results, file = file_results, row.names = FALSE)

#print a simple overview of how this worked per site
overview <- setNames(aggregate(data = all_sites_results, has_xml ~ site_long, mean), c('site', 'percent_renamable'))
overview$percent_renamable <- overview$percent_renamable*100
print(overview)

#close the logging
#sink(type = "message")
#sink()
