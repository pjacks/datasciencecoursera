rankall <- function ( outcome, num="best") {
  ## Validate outcome by getting the column number for the outcome parameter.
  ## An invalid outcome will be column zero.
  column<-switch (outcome, "heart attack"=11, "heart failure"=17, "pneumonia"=23, 0)
  
  ## If column is zero, we were given an invalid outcome
  if(!column)
    stop ("invalid outcome")
  
  ## Validate the num (i.e rank) parameter
  ## Save the parm (for the "worst"case)
  numAsPassed<-num
  if(num=="best")
    num<-1
  if(!is.numeric(num) & !num=="worst")
    stop("invalid num parm - best, worst or numeric")
  
  ## Read the outcomes file
  outcome<-read.csv("outcome-of-care-measures.csv",colClasses="character")
  
  ## ensure our column is numeric, and suppress NA coercion warning
  suppressWarnings(outcome[,column]<-as.numeric(outcome[,column]))

  ## Get the states from the data
  stateslist<-unique(outcome$State[])
  stateslist<-stateslist[order(stateslist)]
  result<-vector()
  
  for (state in stateslist) {
        
    StateOutcomes<-outcome[!is.na(outcome[column]) & outcome[7]==state,]
    
    if(numAsPassed=="worst")
      num<-nrow(StateOutcomes)
  
    ##Sort
    StateOutcomes<-StateOutcomes[order(StateOutcomes[column],StateOutcomes[2]),]
    ## Get the hospital(s) having that rank
    result<-c(result,StateOutcomes[num,2])
  }
  
  return(data.frame(hospital=result,state=stateslist, row.names=stateslist))
  
}