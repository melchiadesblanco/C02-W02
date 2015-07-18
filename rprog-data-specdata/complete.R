complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  ##all_files_name <- list.files(directory, '*.csv', T, T)
  ##data_frame <- data.frame()
  
  if (is.na(directory) || is.na(id)) {
    NA
  } else {
    all_files_name <- list.files(directory, '*.csv', T, T)
    
    if (length(all_files_name) > 0) { 
      completecase_frame <- data.frame()
      
      for (i in id) {
        nobs <- sum(complete.cases(read.csv(all_files_name[i],T)))
        completecase_frame <-
          rbind(completecase_frame, data.frame(i,nobs))
      }
      completecase_frame
    } else NA
  }
}