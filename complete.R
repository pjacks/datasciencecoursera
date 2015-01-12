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
  
  ##List all the files
  ##files<-dir(directory,pattern="*.csv",full.names = TRUE)
  
  # the results by sapply-ing the function foreach monitor
  results<-sapply(X = id,function(id) getCompleteResults(directory, id))
  # Not sure why but results need transposing
  results2 <- t(results)
  # return a data frame from this matrix
  data.frame(results2)
  
}

getCompleteResults<- function (directory, id = 1:332){
  
  # Build the full path, expanding id to length 3 with leading zeros
  filename<-paste(directory,"/",formatC(id,flag="0",width=3),".csv",sep="")
  # Get the data
  allobs<-read.csv(filename)
  # Just the complete results, via a logical vector for rows, all columns
  completeobs<-allobs[!is.na(allobs["sulfate"]) & !is.na(allobs["nitrate"]), ]
  #return id and count
  c(id=id,nobs=nrow(completeobs))
  
}

