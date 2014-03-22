corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to cmpute the correlation between
        ## nitrate and sulfate; the default is 0

        ## Return a numeric vector of correlations


    completeTable <- complete(directory)
    correlations <- vector()

    for (i in completeTable[completeTable$nobs > threshold, ]$id) {
        entry <- getmonitor(i, directory)
        correlations <- c(correlations, cor(entry$sulfate, entry$nitrate,
                                            use="complete.obs"));
    }
    correlations
}