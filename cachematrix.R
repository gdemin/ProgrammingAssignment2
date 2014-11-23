###### Usage ######
# set.seed(123)
# usual_matrix = matrix(sample(1:9),3)
# cache_matr = makeCacheMatrix(usual_matrix) # create object
# 
# cacheSolve(cache_matr) # first time solving
# cacheSolve(cache_matr) # return cached results


# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x) {
    m <- NULL
    # setter for raw matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    # getter for raw matrix
    get <- function() x
    
    # setter for inversed matrix
    setinverse <- function(inverse) m <<- inverse
    
    # getter for inversed matrix
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# This function computes the inverse of the special "matrix" returned by
# makeCacheMatrix above. If the inverse has already been calculated (and the
# matrix has not changed), then the cachesolve should retrieve the inverse from
# the cache.
cacheSolve <- function(x) {
    ## Return a matrix that is the inverse of 'x'
    
    # if result is cached return it
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)  
    }
    
    # if there is no cached result
    data <- x$get() # get original matrix
    m <- solve(data) # solve matrix
    x$setinverse(m) # cache result
    m # return inversed matrix
}


