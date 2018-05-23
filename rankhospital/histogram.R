## plot 30 day mortality rate for heart attack

setwd("/Volumes/david's home/Coursera/Data Science/rankhospital")

outcomedata <- read.csv ("outcome-of-care-measures.csv")

outcomedata[, 11] <- as.numeric(outcomedata[,11])

hist(outcomedata[,11])