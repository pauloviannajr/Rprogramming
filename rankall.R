rankall <- function(outcome, num = "best") {
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
        if (!(outcome %in% c("heart attack","heart failure","pneumonia"))) {
                stop("invalid outcome")
        } 
        
        
        ## For each state, find the hospital of the given rank
        if (outcome == "heart attack") column <- 11
        if (outcome == "heart failure") column <- 17
        if (outcome == "pneumonia") column <- 23
        states.list <- split(outcome.data[,column],outcome.data[,7])
        states.list.no.na <-list()
        for (i in 1:length(states.list)) states.list.no.na[[i]] <- states.list[[i]][!is.na(states.list[[i]])]
        hospital.list <- split(outcome.data[,2],outcome.data[,7])
        if (num == "best") { 
                num = rep(1,length(states.list))
        } else if (num == "worst") {
                num = sapply(states.list.no.na,length)
        } else num = rep(num,length(states.list))
        for (i in 1:length(states.list)) position[i] <- mapply(order,states.list,hospital.list)[[i]][num[i]]
        position<-as.numeric(position)
        
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        
        answer<-NULL
        for (i in 1:length(states.list)) {
                answer <- rbind(answer, c(hospital.list[[i]][position[i]],levels(outcome.data[,7])[i]))
        }
        answer<- as.data.frame(answer)
        names(answer)<-c("hospital","state")
        row.names(answer)<-levels(outcome.data[,7])
        answer
        
        
}
