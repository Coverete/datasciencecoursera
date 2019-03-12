source("rankhospital.R")

rankall <- function(outcome, num = "best") {
    # Read data
    outcome_data <- read.csv("data/outcome-of-care-measures.csv", colClasses = "character")
    
    # Make array of named indeces
    outcome_names <- c("heart attack", "heart failure", "pneumonia")
    outcome_l_indeces <- c(11, 17, 23)
    names(outcome_l_indeces) <- outcome_names
    
    # Check states and outcomes
    
    if (!(outcome %in% outcome_names)){
      stop('invalid outcome')
    }
    
    # Get all unique states
    state_names <- levels(factor(outcome_data[, 7]))
    hospital_names <- c()
    
    for (state in state_names) {
      hospital_names <- c(hospital_names, rankhospital(state, outcome, num))
    }
    df <- data.frame(hospital = hospital_names, state = state_names)
    df
}