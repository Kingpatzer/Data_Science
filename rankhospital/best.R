


best <- function (state, outcome) {

    
    ## read outcome data
    ## note that na.strings
    
    ## it would also be possible to use as.numeric and suppresswarnings ..
    ## such as: 
    ## outcomedata <- suppressWarnings(as.numeric(..
    ##
    ## and split out different columns then re-assemble the data
    ## but this is easier since the data is fairly regular
    
    outcomedata <- read.csv ("outcome-of-care-measures.csv", na.strings=c("Not Available"))
    
    ## validate that the state is in the data set
    
    if(!(state %in% levels(outcomedata[,7]))) {
        stop ("invalid state")
    }
    
    ## check if outcome is in outcome data
    
    valid_outcomes <- c("heart attack", "heart failure",
                        "pneumonia")
    
    if (!(outcome %in% valid_outcomes)) {
        stop ("invalid outcome")
    }
    
    ## determine which column we will get data from
    ## note, since we know outcome is valid, no need
    ## to explicitely check last value
    
    if ("attack" %in% outcome) {
        col <- 11
    } else if ("failure" %in% outcome) {
        col <- 17
    } else {
        col <- 23
    }
    

    ## subset the data to the relevant columns 
    outcomedata <-outcomedata[outcomedata$State == state,c(2,7,col)]
    
    ## rename columns to make things easier
    names(outcomedata)[1] <- "hospital"
    names(outcomedata)[2] <- "state"
    names(outcomedata)[3] <- "value"
    
    ## order the data
    
    ## first put into alphabetical order for handling ties
    outcomedata <- outcomedata[order(outcomedata[,"hospital"], decreasing=FALSE),]
    
    ## now put into order based on "value"
    outcomedata <- outcomedata[order(outcomedata[,"value"], decreasing=FALSE),]
    
    ## select the answer
    as.character(outcomedata$hospital[1])
    
}

