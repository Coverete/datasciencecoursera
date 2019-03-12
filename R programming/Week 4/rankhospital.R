rankhospital <- function(state, outcome, num = "best"){
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
    data = data[!is.na(data$Deaths),]
    
    # if num > numrows then return NA
    if(class(num) == "numeric" && num > nrow(data)){
        NA
    }
    else{
        data = data[order(data$Deaths, data$Name),]
        if(class(num) == "character") {
          if(num == "best") {
            return (data$Name[1])
          }
          else if(num == "worst") {
            return (data$Name[nrow(data)])
          }
          else {
            stop('invalid num')
          }
        }
        else {
          return (data$Name[num])
        }
    }
}