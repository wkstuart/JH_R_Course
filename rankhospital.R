rankhospital <- function(state, outcome, num = "best") {
      
      
      isValidOutcome <- function(outcome) {
            OK <- FALSE
            for (i in 1:length(validOutcome)) {
                  if (outcome == validOutcome[i]) {OK <- TRUE; break}
            }
            OK
      }
      
      isValidState <- function(stateAbbr) {
            OK <- FALSE
            for (i in 1:length(validState)) {
                  if (stateAbbr == validState[i]) {OK <- TRUE; break}
            }
            OK
      }
      
      ## Read outcome data
      outcomeDF <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
      
      ## create vectors for use in checking for valid submissions and examining outcomes
      validState <- unique(outcomeDF$State)
      validOutcome <- c('heart attack','heart failure','pneumonia')
      outcomeColNo <- c( 11, 17, 23)
      
      ## Check that selected state and outcome are valid
      
      if (!isValidState(state)) stop("invalid state")
      if (!isValidOutcome(outcome)) stop("invalid outcome")
      
      ## Return hospital with designated rank from a ranked list of hospitals
      ## in that specified state with lowest 30-day death rate for the 
      ## specified outcome
      
      ## extract data for the designated state and order the data frame
      ## by the results of the specified outcome
      
      outcomeDF <- subset(outcomeDF, outcomeDF$State == state)
      for (i in 1:length(validOutcome)) {
            if (outcome == validOutcome[i]) {
                  #
                  # set the column number for the selected outcome variable
                  colNo = outcomeColNo[i]
                  #
                  # the column number for the hospital name is 2
                  #
                  # The call to is.numeric() will warn of NAs in coercion.
                  # Since the warning is of no consequence in this case
                  # disable warnings when this call is made.
                  #
                  options(warn=-1)
                  #
                  # add a column to the data frame with the selected outcome data
                  # in numeric form
                  #
                  outcomeDF[,colNo] <- as.numeric(outcomeDF[,colNo])
                  #
                  # restore warnings
                  #
                  options(warn=0)
                  #
                  # rank the data frame by the selected outcome column
                  #
                  outcomeDF <- outcomeDF[, c(2,colNo)][ order(outcomeDF[,colNo], outcomeDF[,2]), ]
                  #
                  # drop rows with NA in the outcome variable
                  #
                  outcomeDF <- outcomeDF[complete.cases(outcomeDF),]
                  break
            }
      }
      
      ##print(paste(outcomeDF[,1],outcomeDF[,2]))
      ##
      ## return the hospital with the designated rank 
      ##
      theRank <- 0
      if (num == 'best') {theRank = 1}
      if (num == 'worst') {theRank = length(outcomeDF[,2])}
      if (theRank == 0) {theRank <- num}
      if (theRank > length(outcomeDF[,2])) {
            theHospitalName <- NA
      } else {
            theHospitalName <- outcomeDF[theRank,1]
      }            
      theHospitalName
}
