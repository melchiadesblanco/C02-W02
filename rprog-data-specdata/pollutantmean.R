pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  ## NOTE: Do not round the result!
  
  if(is.na(directory) || is.na(pollutant) || is.na(id) ) {
    NA
  } else {
    all_files_name <- list.files(directory, '*.csv', T, T)
    
    if (length(all_files_name) > 0) { 
      data_frame <- data.frame()
      
      for (i in id) {
        data_frame <- rbind(data_frame, read.csv(all_files_name[i]))
      }
    
      if (pollutant == 'sulfate') {
        result <- mean(data_frame$sulfate, na.rm = TRUE)
      } else if (pollutant == 'nitrate') {
        result <- mean(data_frame$nitrate, na.rm = TRUE)
      } else result <- NA
      
      result
    } else NA
  }
}

