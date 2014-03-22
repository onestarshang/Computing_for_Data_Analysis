rankall <- function(outcome, num = "best") {
  oc <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
  result <-data.frame(hospital=character(),state=character()) # empty data frame
  
  ## Read outcome data
  df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  states <- sort(unique(df$State))
  
  ## Check that state and outcome are valid
  if( is.na(oc[outcome]) ) stop( "invalid outcome" )
  
  for(s in states) {
    #print( s )
    specific.state <- subset(df, State==s)
    specific.state[specific.state=="Not Available"] <- NA
    specific.state <- subset(specific.state, !is.na(specific.state[, oc[outcome]]))
  
    length <- nrow(specific.state)
    #print( c(s, length) )
  
    if( num == "best") {
      rank <- 1
    }
    else if( num == "worst") {
      rank <- length
    }
    else {
      rank = num
    }
  
    if( as.numeric(rank) > as.numeric(length) ) {
      result <- rbind( result, data.frame( hospital=NA, state=s ))
      print( c("rank > lenght:", rank, length ) )
    }
    else {
      ## Return hospital name in that state with the given rank
      ## 30-day death rate
      ordered <- specific.state[ order( specific.state$Hospital.Name ), ]
      ordered <- ordered[ order( as.numeric( ordered[,oc[outcome]] )), ]
      ordered$Rank <- 1:nrow(ordered)
    
      print( c( ordered[ ordered$Rank == rank, ]$Hospital.Name, s ) )
      result <- rbind( result, 
                       data.frame( hospital=ordered[ ordered$Rank == rank, ]$Hospital.Name, state=s) )
    }
  }
  
  return( result )
}