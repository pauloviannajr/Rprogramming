best <- function(state, outcome) {
        ## Read outcome data
        curr.dir <- getwd()
        setwd("C:\\Users\\paulofv\\coursera\\Curso_R")
        outcome.data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        setwd(curr.dir)
        options(warn=-1)
        outcome.data[,11] <- as.numeric(outcome.data[,11])
        outcome.data[,17] <- as.numeric(outcome.data[,17])
        outcome.data[,23] <- as.numeric(outcome.data[,23])
        outcome.data[,7] <- as.factor(outcome.data[,7])
        options(warn=0)
        
        ## Check that state and outcome are valid
        if (!(state %in% outcome.data[,7])) { 
                stop("invalid state")
        } 
        if (!(outcome %in% c("heart attack","heart failure","pneumonia"))) {
                stop("invalid outcome")
        } 
      

        ## Return hospital name in that state with lowest 30-day death
        ## rate
        if (outcome == "heart attack") column <- 11
        if (outcome == "heart failure") column <- 17
        if (outcome == "pneumonia") column <- 23  
        best.rates <- tapply(outcome.data[,column],outcome.data[,7],min,na.rm=T)
        hosp.names <- outcome.data[outcome.data$State==state & outcome.data[,column]==best.rates[state] & !is.na(outcome.data[,column]),2]
        hosp.names <- sort(hosp.names)
        return(hosp.names[1])
}
