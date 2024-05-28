#' Functions needed for retrospectively correcting badly renamed Batlogger audio files
#' Written by Simon Gillings, BTO
#' 13 March 2024


#' Check a WAV and associated XML and suggest new file names
#' 
#' @param this_wav = the full path and name of a wav file 
#' @return a single line dataframe containing diagnostics and proposed new file names
#' 
#' @details This looks for an XML file. If it exists (and is not corrupt) it reads 
#' the file to extract recording datetime and lat-long if possible. These are used 
#' to propose a new 'good' filename. The function also produces a 'bad' filename 
#' as was incorrectly created by an early version of the Shiny app, which was a 
#' concatenation of folder path and filename.
#' 
process_a_file <- function(this_wav) {
  this_dirname <- dirname(this_wav)
  this_file <- basename(this_wav)
  
  #check for XML
  #is there an XML file?
  datetime_value <- NA
  position_value <- NA
  dt <- NA
  lat_xml <- NA
  lon_xml <- NA
  dt_xml <- NA
  file_xml <- gsub(".wav|.WAV", ".xml", this_wav)
  if(!file.exists(file_xml)) {
    has_xml <- FALSE
  }
  #if XML exists, read it and find the datetime value
  if(file.exists(file_xml)) {
    
    #check if the file can be read
    xml_corrupt <- tryCatch({
      # Attempt to read the file
      data <- read_xml(file_xml)
      0
    }, error = function(e) {
      # Handle the error
      1
    })        
    
    #if the xml is corrupt...
    if(xml_corrupt==1) {
      has_xml <- FALSE
    }
    
    #if the xml is readable, proceed
    if(xml_corrupt==0) {
      has_xml <- TRUE
      doc <- read_xml(file_xml)
      datetime_value <- xml_text(xml_find_first(doc, "//DateTime"))
      position_value <- xml_text(xml_find_first(doc, "//Position"))
      if(!is.na(datetime_value)) {
        dt <- strptime(datetime_value, format = "%d.%m.%Y %H:%M:%S")
        dt_xml <- format(dt, "%Y%m%d_%H%M%S")
      }
      if(!is.na(position_value)) {
        lat_xml <- round(as.numeric(stringr::str_split_fixed(position_value, " ", Inf)[1]), 4)
        lon_xml <- round(as.numeric(stringr::str_split_fixed(position_value, " ", Inf)[2]), 4)
      }
    }
  }
  
  #file is renamable if there is xml and date can be extracted 
  #no date extracted so cannot be renamed
  if(is.na(dt_xml)) {
    newname_good <- NA
  }
  #has date extracted so can be renamed - check if rename with lat long
  if(!is.na(dt_xml)) {
    #if lat and lon both available, use them
    if(!is.na(lat_xml) & !is.na(lon_xml)) {
      #convert lat-long to character and replace decimal point with ~ as required for some batviewer apps
      lat <- gsub("\\.", "~", as.character(lat_xml))
      lon <- gsub("\\.", "~", as.character(lon_xml))
      #compile new filename in Pipeline format
      newname_good <- trimws(paste0(lat, "+", lon, "_", dt_xml, ".wav"))
    }
    #if either lat or long unavailable use datetime renaming with old filename suffix
    if(is.na(lat_xml) | is.na(lon_xml)) {
      newname_good <- trimws(paste0(dt_xml,"_",this_file))
    }
  }
  
  #recreate what the incorrectly named file would be called
  newname_bad <- trimws(paste(basename(this_site), basename(this_dirname), this_file, sep = '_'))
  
  output <- data.frame(original_audio = this_wav,
                       original_audio_path = this_dirname,
                       original_audio_file = this_file,
                       has_xml = has_xml,
                       date_from_xml = dt_xml,
                       lat_from_xml = lat_xml,
                       lon_from_xml = lon_xml,
                       newname_good = newname_good,
                       newname_bad = newname_bad)
  
  return(output)
}


#' Function to rename a wav file using datetime and latlong
#' 
#' @param this_wav = full path for a wav file to be renamed
#' 
#' @return string of c('renamed', 'failed')
#' 
rename_a_wav_file <- function(this_wav) {
  #get the filename
  filename <- basename(this_wav)

  #is the file already renamed
  already_done <- grepl("\\d{2}~\\d{4}\\+", filename)
  if(already_done==TRUE) {
    outcome <- 'already renamed'
    return(outcome)
  }
  
  #find where Location bit starts and prune off any part number before that
  prune <- gregexpr('Location', filename)[[1]][1]
  name_to_match <- substr(filename,prune, nchar(filename))
  
  #find the row of the naming info that relates to this file
  this_naming_info <- names[which(grepl(name_to_match, names$newname_bad, fixed = TRUE)),]

  #if this returns none, or more than one file, throw a warning and jump to next file
  if(nrow(this_naming_info)!=1) {
    warning(paste(this_wav, 'has', nrow(this_naming_info), 'matches'))
    outcome <- 'cannot match filename'
    return(outcome)
  }
  
  #if only one match, potentially do the renaming
  if(nrow(this_naming_info)==1) {
    wav_old <- this_wav
    #check that newname_good isn't NA (indicating no info on which to rename)
    if(!is.na(this_naming_info$newname_good)) {
      wav_new <- file.path(dirname(this_wav), this_naming_info$newname_good)
      temp <- file.rename(wav_old, wav_new)
      outcome <- ifelse(temp==TRUE, 'renamed', 'failed to rename')
    }
    #no newname info to rename the file
    if(is.na(this_naming_info$newname_good)) {
      outcome <- 'no date info for renaming'
    }
    return(outcome)
  }
}


#' Function to rename an xml file using datetime and latlong
#' 
#' @param this_xml = full path for a xml file to be renamed
#' 
#' @return string of c('renamed xml', 'failed')
#' 
rename_a_xml_file <- function(this_xml) {
  #get the filename
  filename <- basename(this_xml)
  #create the paired wav name
  this_wav <- gsub('xml','wav',filename)
  
  #is the file already renamed
  already_done <- grepl("\\d{2}~\\d{4}\\+", filename)
  if(already_done==TRUE) {
    outcome <- 'already renamed'
    return(outcome)
  }
  
  
  name_to_match <- file.path(basename(dirname(this_xml)), this_wav)
  
  #find the row of the naming info that relates to this file
  #can't do just on file name as this restarts each night. So needs to be on folder too
  this_naming_info <- names[which(grepl(name_to_match, names$original_audio, fixed = TRUE)),]
  
  #if this returns none, or more than one file, throw a warning and jump to next file
  if(nrow(this_naming_info)!=1) {
    warning(paste(this_xml, 'has', nrow(this_naming_info), 'matches'))
    outcome <- 'cannot match xml filename'
    return(outcome)
  }
  
  #if only one match, potentially do the renaming
  if(nrow(this_naming_info)==1) {
    xml_old <- this_xml
    #check that newname_good isn't NA (indicating no info on which to rename)
    if(!is.na(this_naming_info$newname_good)) {
      xml_new <- gsub('wav','xml',file.path(dirname(this_xml), this_naming_info$newname_good))
      temp <- file.rename(xml_old, xml_new)
      outcome <- ifelse(temp==TRUE, 'renamed xml', 'failed to rename')
    }
    if(is.na(this_naming_info$newname_good)) {
      outcome <- 'no date info for renaming'
    }
    return(outcome)
  }
}


#' fix the names in a csv
#' @details Read the csv file and try to replace the old bad filenames in the 
#' original file name column with the corrected file names. Once this is done it
#' updates the date and time fields with new information based on the correct
#' file name
#' 
#' @param this_csv = full path to a csv of Pipeline results
#' 
#' @return numeric the percent of rows in the csv that were fixed
#' 
fix_a_csv <- function(this_csv) {
  #read the csv. Note some have a missing end of line feed and this needs to be added first
  csv_contents <- tryCatch({
    # Attempt to read the file
    read.csv(this_csv)
  }, error = function(e) {
    # Handle the error
    cat('Append EOF\n')
    cat("\n", file = this_csv, append = TRUE)
    read.csv(this_csv)
  })         
  
  
  #iterate over rows in the results dataframe and replace the bad filename with good filename
  outcomes <- list()
  for(r in 1:nrow(csv_contents)) {
    done <- 0
    original <- csv_contents$ORIGINAL.FILE.NAME[r]
    prune <- gregexpr('Location', original)[[1]][1]
    #find where Location bit starts and prune off any part number before that
    name_to_match <- substr(original,prune, nchar(original))
    
    #find the row of the naming info that relates to this file
    this_naming_info <- names[which(names$newname_bad == name_to_match, fixed = TRUE),]
    
    #if only one match, do the renaming and datetime fixes
    if(nrow(this_naming_info)==1) {
      #if the newname is NA (no XML) skip
      if(is.na(this_naming_info$newname_good)) {
        done <- 0
      }
      #if the newname is OK
      if(!is.na(this_naming_info$newname_good)) {
        csv_contents$ORIGINAL.FILE.NAME[r] <- this_naming_info$newname_good
        
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
        
        #update the relevant fields
        csv_contents$ACTUAL.DATE[r] <- actual_date_str
        csv_contents$SURVEY.DATE[r] <- survey_date_str
        csv_contents$TIME[r] <- time_str
        done <- 1
      }      
      
      #return the status
      outcomes[r] <- done
    }
  }
  write.csv(csv_contents, file = this_csv, row.names = FALSE)
  out <- mean(unlist(outcomes))*100
  return(out)
}
