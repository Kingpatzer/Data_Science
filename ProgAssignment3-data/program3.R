setwd("/Volumes/david's home/Coursera/Data Science/ProgAssignment3-data")
rm(list=ls())

### plot the 30-day mortality rates for heart attacks

## read in CSV

outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

## convert outcome to numeric

outcome[, 11] <- as.numeric((outcome[,11]))

## create historgram
hist(outcome[,11])

### function to find the best hospital in a state based on outcome of care 

best <- function(state, outcome) {
        ## read outcome data
        
}