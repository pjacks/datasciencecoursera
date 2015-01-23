best <- function (state, outcome) {
  ## Validate outcome by getting the column number for the outcome parameter.
  ## An invalid outcome will be column zero.
  column<-switch (outcome, "heart attack"=11, "heart failure"=17, "pneumonia"=23, 0)
  
  ## If column is zero, we were given an invalid outcome
  if(!column)
    stop ("invalid outcome")
  
  ## Read the outcomes file
  outcome<-read.csv("outcome-of-care-measures.csv",colClasses="character")
  
  ## Validate the state by ensuring > 0 obs for that state code
  if (!nrow(outcome[outcome$State==state,]))
      stop ("invalid state")
  
  ## ensure our column is numeric, and suppress NA coercion warning
  suppressWarnings(outcome[,column]<-as.numeric(outcome[,column]))
  
  ## Get the observations for this state
  StateOutcomes<-outcome[outcome$State==state,]
  
  ## Find the minimum
  min<-min(StateOutcomes[,column],na.rm = TRUE)
  
  ## Get the hospital(s) having that minimum
  h<-as.character(StateOutcomes[StateOutcomes[,column]==min & !is.na(StateOutcomes[,column]) ,2])
  
h
  
  
}