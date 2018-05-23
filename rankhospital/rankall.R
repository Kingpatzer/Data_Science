library(dplyr)

rankall <- function(outcome, num = "best") {
    
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    
    outcome <- "heart attack"
    
    ## read outcome data
    ## note that na.strings
    ## it would also be possible to use as.numeric and suppresswarnings ..
    ## such as: 
    ## outcomedata <- suppressWarnings(as.numeric(..
    ##
    ## and split out different columns then re-assemble the data
    ## but this is easier since the data is fairly regular
    
    outcomedata <- read.csv ("outcome-of-care-measures.csv", na.strings=c("Not Available"))
    
    ## check if outcome is in outcome data
    
    valid_outcomes <- c("heart attack", "heart failure",
                        "pneumonia")
    
    if (!(outcome %in% valid_outcomes)) {
        stop
    }
    
    if ("attack" %in% outcome) {
        col <- 11
    } else if ("failure" %in% outcome) {
        col <- 17
    } else {
        col <- 23
    }
    
    
    ## subset the data to the relevant columns 
    outcomedata <-outcomedata[c(2,7,col)]
    
    ## rename columns to make things easier
    names(outcomedata)[1] <- "hospital"
    names(outcomedata)[2] <- "state"
    names(outcomedata)[3] <- "value"
    
    ## order the data
    #outcomedata[complete.cases(outcomedata),] %>% arrange(value) %>% arrange(state) %>% mutate(rank = rank(desc(value))) %>% filter(rank == 3)
    
    outcomedata <- outcomedata[complete.cases(outcomedata),]
    
    splitbystate = split(outcomedata, outcomedata$state)
    answer = lapply(splitbystate, function(x, num) {
        # Order by Deaths and then HospitalName
        x = x[order(x$value, x$hospital),]
        
        # Return
        if(class(num) == "character") {
            if(num == "best") {
                return (x$hospital[1])
            }
            else if(num == "worst") {
                return (x$hospital[nrow(x)])
            }
        }
        else {
            return (x$hospital[num])
        }
    }, num)
    
    
    return (answer)
    
    
}
