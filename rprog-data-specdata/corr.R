corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  ## NOTE: Do not round the result!

  
  ##source("complete.R")
    
  if(is.na(directory) || is.na(threshold) ) {
    NA
  } else {
    cases_frame <- complete(directory)
    cases_greater <- cases_frame[cases_frame$nobs > threshold,1]
    all_file_names <- list.files(path = directory,'*.csv', T, T)
    correlations <- rep(NA,length(cases_greater))
    for (i in cases_greater) {
      fileData <- (read.csv(all_file_names[i]))
      cases_frame <- complete.cases(fileData)
      v_Sulfate <- fileData[cases_frame, 2]
      v_Nitrate <- fileData[cases_frame, 3]
      correlations[i] <- cor(x = v_Sulfate, y = v_Nitrate)
    }
    correlations <- correlations[complete.cases(correlations)]
    if (length(correlations) == 0) {
      returnvalue <- numeric()
    }
    else {
      returnvalue <- correlations
    }
    returnvalue
  }
}