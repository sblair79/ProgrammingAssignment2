## This pair of functions caches the inverse of a matrix and, instead of
## re-solving for the inverse each time, checks the cache first and retrieves
## the inverse from the cache if it is there, only solving for the inverse
## if it is not in the cache.

## This function takes a matrix as its argument, defines the get, 
## setinverse and getinverse functions, and returns a list of these functions.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL  # initializes variable for inverse of x, sets value to null
        get <- function() x         # defines function that returns x
        setinverse <- function(inverse) i <<- inverse  # binds inverse to i 
        getinverse <- function() i  # defines function that returns i
        list(get = get,             # creates list of three functions 
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function returns the inverse of the "matrix" object returned by
## makeCacheMatrix. It checks the cache first and if the cache contains a value
## for the inverse already, it returns that value; otherwise it solves for the
## inverse, stores that value in the cache, and returns it.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()  
        if(!is.null(i)) {  # if i is not null, retrieves i from cache
                message("getting cached data")  # indicates pulling from cache
                return(i)  
        }                  # otherwise, if i is null, executes following:
        data <- x$get()    # assigns original matrix to variable data
        i <- solve(data, ...)  # computes inverse and assigns to i
        x$setinverse(i)    # sets value of inverse in cache
        i                  # returns inverse
}