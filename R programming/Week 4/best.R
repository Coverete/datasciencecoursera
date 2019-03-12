best <- function(state, outcome){
    # Read data
    outcome_data <- read.csv("data/outcome-of-care-measures.csv", colClasses = "character")

    # Make array of named indeces
    outcome_names <- c("heart attack", "heart failure", "pneumonia")
    outcome_l_indeces <- c(11, 17, 23)
    names(outcome_l_indeces) <- outcome_names
    
    # Check states and outcomes
    if (!(state %in% outcome_data$State)){
        stop('invalid state')
    } 
    
    if (!(outcome %in% outcome_names)){
        stop('invalid outcome')
    }
    
    # Get current outcome index
    cur_outcome <- outcome_l_indeces[[outcome]]
    
    # Filter by state and outcome
    outcome_data <- outcome_data[outcome_data$State==state, ]
    data <- outcome_data[, c(2, cur_outcome)]
    
    # Rename columns
    names(data) <- c("Name", "Deaths")
    data$Deaths <- as.numeric(data$Deaths)

    # Get min
    data <- data[order(data$Deaths, data$Name),]
    data$Name[1]
}
