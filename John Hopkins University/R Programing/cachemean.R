## cachemean.R
##
## Get mean of a cached Vector
##
## Usage:
##  v <- makeVector(c(1, 2, 3, 4))
##  cachemean(v)
cachemean <- function(x, ...) {
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}
