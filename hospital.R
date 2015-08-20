current_directory <- getwd()
setwd("C:\\Users\\paulofv\\coursera\\Curso_R")

outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)

ncol(outcome)
nrow(outcome)
names(outcome)

outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11])

## "heart attack" 11, "heart failure" 17, or "pneumonia" 23

names(outcome[,c(11,17,23)])
