best <- function(state, outcome) {
      
      
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

      # create vectors for use in checking for valid submissions and examining outcomes
      validState <- unique(outcomeDF$State)
      validOutcome <- c('heart attack','heart failure','pneumonia')
      outcomeColNo <- c( 11, 17, 23)
      
      ## Check that selected state and outcome are valid
      
      if (!isValidState(state)) stop("invalid state")
      if (!isValidOutcome(outcome)) stop("invalid outcome")
      
      ## Return hospital name in that state with lowest 30-day death rate

      outcomeDF <- subset(outcomeDF, outcomeDF$State == state)
      for (i in 1:length(validOutcome)) {
            if (outcome == validOutcome[i]) {
                  #
                  # The call to is.numeric() will warn of NAs in coercion.
                  # Since the warning is of no consequence in this case
                  # disable warnings when this call is made.
                  options(warn=-1)
                  #
                  outcomeDF$SelectedOutcome <- as.numeric(outcomeDF[,outcomeColNo[i]])
                  #
                  # restore warnings
                  options(warn=0)
                  #
                  minValue <- min(outcomeDF$SelectedOutcome, na.rm=T)
                  break
            }
      }
      minValue
      selectedHospitals <- subset(outcomeDF, outcomeDF$SelectedOutcome == minValue)
      hospitalNames <- selectedHospitals$Hospital.Name
      if (length(hospitalNames) > 1) {
            # alphabetize the names and select only the first
            ##message(paste('There are more than one hospitals with the same result in ', state, '.', sep=''))
            hospitalNames <- sort(hospitalNames)
      }
      hospitalNames[1]
}

