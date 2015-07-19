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
  
  numStation <- length(id); # number of stations/files to be used
  result <- data.frame(id=1:numStation, nobs=1:numStation)
  
  for(i in 1:numStation)
  {
    stationId <- sprintf("%03d", id[i]); # Current station ID
    filename <- paste(directory,'/',stationId,".csv", sep=''); # Name of the file to be read
    
    rawData <- read.csv(filename) # read the file
    tt <- complete.cases(rawData)
    ss <- table(tt)
    
    if(is.na(id[i]))
    {
      print(i)
    }
    
    result[i,1] <- id[i]
    result[i,2] <- ss[2]
    #fullData <- rbind.data.frame(fullData,rawData) # append data from different stations
  }
  
  result
}