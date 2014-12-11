rankall <- function(outcome, num = "best") {
      
      
      isValidOutcome <- function(outcome) {
            OK <- FALSE
            for (i in 1:length(validOutcome)) {
                  if (outcome == validOutcome[i]) {OK <- TRUE; break}
            }
            OK
      }
      
      ## Read outcome data
      DF <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
      
      ## create vectors for use in checking for valid submissions and examining outcomes
      allStates <- unique(DF$State)
      validOutcome <- c('heart attack','heart failure','pneumonia')
      outcomeColNo <- c( 11, 17, 23)
      
      ## Check that outcome is valid
      
      if (!isValidOutcome(outcome)) stop("invalid outcome")
      
      ## rank hospitals by state and outcome
      
      ## get the column number for the outcome variable as colNo
      ## the column number for Hospital.Name is 2
      ## the column number for State is 7
      
      for (i in 1:length(validOutcome)) {
            if (outcome == validOutcome[i]) {
                  #
                  # set the column number for the selected outcome variable
                  colNo = outcomeColNo[i]
                  break
            }
      }
      ##
      ## the column number for the hospital name is 2
      ##
      ## The call to is.numeric() will warn of NAs in coercion.
      ## Since the warning is of no consequence in this case
      # disable warnings when this call is made.
      ##
      options(warn=-1)
      ##
      # add a column to the data frame with the selected outcome data
      # in numeric form
      ##
      DF[,colNo] <- as.numeric(DF[,colNo])
      ##
      # restore warnings
      ##
      options(warn=0)
      ##
      ## extract the three columns required 
      ## col 1 will be Hospital.Name, col 2 will be State, col 3 will be outcome
      ##
      DF <- DF[, c(2, 7, colNo)]
      
      ## determine the rank that should be returned
      theRank <- 0
      if (num == 'best') {theRank = 1}
      if (num == 'worst') {theRank = length(DF[,2])}
      if (theRank == 0) {theRank <- num}
      
      outputDF <- data.frame(State=character(), Hospital=character(), stringsAsFactors=FALSE)       
      for (i in 1:length(allStates)) {
            stateDF <- subset(DF, DF$State == allStates[i])
            ##
            # rank the data frame by the selected outcome column
            ##
            stateDF <- stateDF[ order(stateDF[,3]), ]
            ##
            ## drop rows with NA in the outcome variable
            ##
            stateDF <- stateDF[complete.cases(DF),]
            
            if (theRank > length(stateDF[,2])) {
                  theHospitalName <- NA
            } else {
                  theHospitalName <- stateDF[theRank,1]
            }
            outputDF = rbind(outputDF,c(allStates[i], theHospitalName))
      }
      outputDF
}