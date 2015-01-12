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
  
  AllMonitors<-sapply(id, function(id) pollutantData(directory, pollutant, id))
  AllMonitorsFlat<-unlist(AllMonitors)
  mean(AllMonitorsFlat)
}

pollutantData <- function (directory, pollutant, id = 1:332)
{
  # Build the full path, expanding id to length 3 with leading zeros
  filename<-paste(directory,"/",formatC(id,flag="0",width=3),".csv",sep="")
  # Get the data
  allData<-read.csv(filename)
  # Get the pollutant we want
  pollData<-allData[pollutant]
  # Strip out the NAs and return
  pollData[!is.na(pollData[pollutant])]
}