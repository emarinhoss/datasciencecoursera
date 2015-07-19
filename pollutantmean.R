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
  
  ##### set directory as working directory #####
  #setwd(directory)
  
  numRows <- 0.
  totSum  <- 0.
  
  numStation <- length(id); # number of stations/files to be used
  fullData <- data.frame()
  
  # loop over the number of stations
  for(i in 1:numStation)
  {
    stationId <- sprintf("%03d", id[i]); # Current station ID
    filename <- paste(directory,'/',stationId,".csv", sep=''); # Name of the file to be read
    
    rawData <- read.csv(filename) # read the file
    fullData <- rbind(fullData,rawData) # append data from different stations
  }
  
  mean(fullData[,pollutant],na.rm = T)
  
}