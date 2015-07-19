corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  ## NOTE: Do not round the result!
  
  res <- complete(directory)
  xt  <- subset(res, nobs > threshold)
  
  numberRows <- nrow(xt)
  
  if(numberRows>0)
  {
    solution <- c(1:numberRows)
    for(i in 1:numberRows)
    {
      stationId <- sprintf("%03d", xt[i,1]); # Current station ID
      if(is.na(xt[i,1]))
      {
        print(xt[i,])
      }
      filename <- paste(directory,'/',stationId,".csv", sep=''); # Name of the file to be read
      
      rawData <- read.csv(filename) # read the file
      gooData <- complete.cases(rawData)
      
      solution[i] <- cor(rawData[gooData,"nitrate"],rawData[gooData,"sulfate"])
    }
  }
  else
  {
    solution <- vector()
  }
  
  
  solution
  
}