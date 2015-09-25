## This is Programming Assignment #2 for https://class.coursera.org/rprog-032
## Provides functionality for caching results of inverting the matrix


## Wraps a matrix into an R that is capable of storing cached inversed matrix along
## with the original matrix

makeCacheMatrix <- function(x = matrix()) {
    im <- NULL

    set <- function(y = matrix) {

        if (is.null(y)){
            stop("Null passed")
        }
        
        if (!is.matrix(y)) stop("Not a matrix")
        
        if (ncol(y) != nrow(y)){
            stop("Not a square matrix")
        }
        
        x <<- y
        im <<- NULL
    }
    
    set(x) # to make sure the checks in set() pass
    
    get <- function() x
    
    setinversed <- function(inversed = matrix()) {
        
        if (is.null(inversed)){
            stop("Null passed")
        }

        if (!is.matrix(inversed)) stop("Not a matrix")
        
        # quick check that the dimensions are correct
        if (ncol(inversed) != nrow(inversed))
        {
            stop("Not a square matrix passed to setinversed")
        }
        
        if (ncol(inversed) != ncol(x))
        {
            stop("Wrong dimensions of inversed matrix passed")
        }
        
        # check that it's really inv of our matrix
        assumed_identity <- x %*% inversed
        
        ok = TRUE
        for(i in seq_along(ncol(assumed_identity)))
            for(j in seq_along(nrow(assumed_identity)))
            {
                if (i == j)
                {
                    # 1.0 on diag
                    if (abs(assumed_identity[i,j] - 1) > 0.000001)
                        ok = FALSE
                }
                else
                {
                    # 0.0 outside of diag
                    if (abs(assumed_identity[i,j]) > 0.000001)
                        ok = FALSE
                }
            }
        
        if (!ok)
            stop("Inversed matrix multiplied by orig matrix doesn't yield Identity matrix")
        
        im <<- inversed
    }
    
    
    getinversed <- function() im
    
    list(set = set, get = get,
         setinversed = setinversed,
         getinversed = getinversed)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    
    if (!is.list(x)) stop("Unexpected input type")
    
    if (is.null(x$getinversed) || is.null(x$get) || is.null(x$setinversed) )
    {
        stop("Unexpected input type")
    }
    
    y <- x$getinversed()
    
    if(!is.null(y)) {
        message("getting cached data")
        return(y)
    }
    
    data <- x$get()
    im <- solve(data, ...)
    x$setinversed(im)
    im
}
