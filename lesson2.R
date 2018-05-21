




pollutantmean <- function(directory, pollutant, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating the location
        ## of the CSV files
        
        ## 'pollutant' is a character vector of length 1 indicating teh
        ## name of the pollutnat for whcih we will calculate the mean;
        ## either 'sulfate' or 'nitrate'
        
        ## 'id' is an integer vector indicating the monitor id numbers
        ## to be used
        
        ## Return the mean of hte pollutatant across all monitors list
        ## in the 'id' vector (ignoring "NA" values)
        ## do not round the result
        
        ## check to ensure entry criteria are met:
        
        
        if ((length(directory) != 1 | !is.character(directory)) |
            (length(pollutant) != 1 | !is.character(pollutant)))  {
                warning(
                        "USAGE: pollutantmean('directory', 'pollutant', id=1:332)
                        Directory and pollutant must both be character vectors of
                        length 1, id must be an integer vector",
                        call. = FALSE
                )
                return()
        }
        
        ## create a ls of csv files
        
        # debug data
        #directory <- "specdata"
        #pollutant <- "nitrate"
        #id <- c(70:72)
        
        data_set <- list()
        my_files <- file.path(directory, list.files(path = directory))
        data_set <- lapply(my_files[id], read.csv)
        
        # keep the columns named by pollutant
        
        pollutant_data <- lapply(data_set, "[", pollutant)
        
        # convert the list of 1 column data.frames to a list of lists
        
        pollutant_data_as_lst <- lapply(pollutant_data, as.list)
        
        # combine the lists into a single vector (actually a 1xn matrix)
        
        final_data <- apply(rbind(pollutant_data_as_lst), 1, unlist)
        
        # compute the mean and return it, ignore NA's
        
        mean(final_data, na.rm = TRUE)
        
}

complete <- function(directory, id = 1:322) {
        ## 'directory' is a character vector of length 1 indicating the location
        ## of the CSV files
        
        ## 'id' is an integer vector indicating the monitor id numbers
        ## to be used
        
        ## returns a data frame of the form:
        
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        
        ## where 'id' is the monitor ID number and 'nobs' is the number of
        ## complete cases
        
        ## create a ls of csv files
        
        # debug data
        #directory <- "specdata"
        #id <- c(70:72)
        
        data_set <- list()
        my_files <- file.path(directory, list.files(path = directory))
        data_set <- lapply(my_files[id], read.csv)
        
        # get the complete cases for each file
        
        results <- lapply(data_set, complete.cases)
        
        # complete.cases creates a logical vector, just sum it up
        
        results <- lapply(results, sum)
        
        #use do.call(rbind ..) to turn the resulting list into a column
        
        return(data.frame(id = id, nobs = do.call(rbind, results)))
        
}

corr <- function (directory, threshold = 0) {
        # return a numberic vector of correlations
        
        data_set <- list()
        my_files <- file.path(directory, list.files(path = directory))
        data_set <- lapply(my_files, read.csv)
        
        # determine which cases meet the threshold
        complete_cases <- complete(directory)$nobs > threshold
        
        # parse down data set to only cases meeting threshold, and remove
        # NA's
        
        # first find which are complete in each data.frame
        corr_data_complete <-
                lapply(data_set[complete_cases], complete.cases)
        
        # now select only that data in each data.frame
        corr_data <- lapply(data_set[complete_cases], drop_na)
        
        # get down to the sulpher and nitrate data
        corr_data <- lapply(corr_data, "[", c(2, 3))
        
        # compute correllation matrixes
        cr <- lapply(as.matrix(corr_data), cor)
        
        # just grab  the nitrate/sulphate value
        cr <- lapply(cr, "[", 2)
        
        # flatten to a numberical vector and return
        cr <- apply(rbind(cr), 2, unlist)
        
        
}

make.NegLogLik <- function (data, fixed = c(FALSE, FALSE)) {
        params <- fixed
        function(p) {
                params[!fixed] <- p
                mu <- params[1]
                sigma <- params[2]
                a <- -0.5 * length(data) * log(2 * pi * sigma ^ 2)
                b <- -0.5 * sum((data - mu) ^ 2) / (sigma ^ 2)
                - (a + b)
        }
}
