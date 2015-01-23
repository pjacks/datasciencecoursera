rankhospital <- function (state, outcome, num="best") {
  ## Validate outcome by getting the column number for the outcome parameter.
  ## An invalid outcome will be column zero.
  column<-switch (outcome, "heart attack"=11, "heart failure"=17, "pneumonia"=23, 0)
  
  ## If column is zero, we were given an invalid outcome
  if(!column)
    stop ("invalid outcome")
  
  ## Validate the num (i.e rank) parameter
  if(num=="best")
    num<-1
  if(!is.numeric(num) & !num=="worst")
    stop("invalid num parm - best, worst or numeric")
  
  ## Read the outcomes file
  outcome<-read.csv("outcome-of-care-measures.csv",colClasses="character")
  
  ## Validate the state by ensuring > 0 obs for that state code
  if (!nrow(outcome[outcome$State==state,]))
      stop ("invalid state")
  
  ## ensure our column is numeric, and suppress NA coercion warning
  suppressWarnings(outcome[,column]<-as.numeric(outcome[,column]))
  
  ## Get the observations for this state
  StateOutcomes<-outcome[outcome$State==state,]
  
  ## Filter out the NAs for that column
  StateOutcomes<-StateOutcomes[!is.na(StateOutcomes[column]),]
  ## If looking for worst, get the last row index
  if(num=="worst")
    num<-nrow(StateOutcomes)
  ## Get the hospital(s) having that rank
  StateOutcomes[order(StateOutcomes[column],StateOutcomes[2]),2][num]
  
  #h<-as.character(StateOutcomes[StateOutcomes[,column]==min & !is.na(StateOutcomes[,column]) ,2])
  

  
  
}