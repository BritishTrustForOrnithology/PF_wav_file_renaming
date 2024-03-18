#' OPTIONAL - fix dates and times in CSVs
#' 
#' For cases where the audio filename in a Pipeline CSV file has been fixed but 
#' the detection date times were not fixed.
#' 
#' Simon Gillings
#' March 2024

library(rstudioapi)
library(lubridate)

#select folder containing CSVs
path_csv <- rstudioapi::selectDirectory(caption = 'Select folder containing the CSV files...')

#list csvs
csvs <- list.files(path = path_csv, pattern = "*.csv", full.names = TRUE)
  
for(i in 1:length(csvs)) {
  this_csv <- csvs[i]
  cat('Fixing:',this_csv,'\n')

  #read the csv. Note some have a missing end of line feed and this needs to be added first
  csv_contents <- tryCatch({
    # Attempt to read the file
    read.csv(this_csv)
  }, error = function(e) {
    # Handle the error
    #cat('Append EOF\n')
    cat("\n", file = this_csv, append = TRUE)
    read.csv(this_csv)
  })         
  
  #iterate over rows in the results dataframe and check/replace the datetimes
  fixdetdates <- list()
  fixdettimes <- list()
  fixsurvdates <- list()
  for(r in 1:nrow(csv_contents)) {
    #print(csv_contents[r,])
    #extract date and time from newly updated original file name
    date <- strsplit(csv_contents$ORIGINAL.FILE.NAME[r], "_")[[1]][2]
    time <- strsplit(csv_contents$ORIGINAL.FILE.NAME[r], "_|\\.")[[1]][3]
    #make datetime object
    datetime <- lubridate::fast_strptime(paste(date, time), format = "%Y%m%d %H%M%S")
    #extract string versions of actual date
    actual_date_str <- strftime(datetime, format = "%d/%m/%Y")
    #extract string versions of actual time
    time_str <- strftime(datetime, format = "%H:%M:%S")
    #extract string versions of survey date (date of start of the night)
    survey_date_str <- strftime(as.Date(ifelse(lubridate::am(datetime), 
                                                  as.Date(datetime)-1, 
                                                  as.Date(datetime)), 
                                           origin = '1970-01-01'), 
                                format = '%d/%m/%Y')
    fixdetdates[[r]] <- ifelse(actual_date_str != csv_contents$ACTUAL.DATE[r], 1, 0)
    fixsurvdates[[r]] <- ifelse(survey_date_str != csv_contents$SURVEY.DATE[r], 1, 0)
    fixdettimes[[r]] <- ifelse(time_str != csv_contents$TIME[r], 1, 0)
    
    #cat('  Replace',csv_contents$ACTUAL.DATE[r],'with',actual_date_str,'\n')
    #cat('  Replace',csv_contents$SURVEY.DATE[r],'with',survey_date_str,'\n')
    #cat('  Replace',csv_contents$TIME[r],'with',time_str,'\n')

    #update the relevant fields
    csv_contents$ACTUAL.DATE[r] <- actual_date_str
    csv_contents$SURVEY.DATE[r] <- survey_date_str
    csv_contents$TIME[r] <- time_str
  }      
  write.csv(csv_contents, file = this_csv, row.names = FALSE)
  fixdetdates <- mean(unlist(fixdetdates))
  fixdettimes <- mean(unlist(fixdettimes))
  fixsurvdates <- mean(unlist(fixsurvdates))
  
  cat(' Changed', 100*fixdetdates, 'percent of detection dates\n')
  cat(' Changed', 100*fixdettimes, 'percent of detection times\n')
  cat(' Changed', 100*fixsurvdates, 'percent of survey dates\n')
}
