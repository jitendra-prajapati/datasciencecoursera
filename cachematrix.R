## This file demonstrate how R function is able to cache potentially time-consuming computations 
## Contains two function "makeCacheMatrix" and "cacheSolve" 

## makeCacheMatrix function takes a matrix which is invertible, along with this it used to get , set , setinverse , getinverse
## e.g   x <- makeCacheMatrix(matrix(c(1:4),nrow=2,ncol=2)) 

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
    set <- function(y) {
                x <<- y
                m <<- NULL
    }
    get <- function() x
    setinverse <- function(mean) m <<- mean
    getinverse <- function() m
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


	## This function compute the inverse of matrix if it not already cached and cached the computed inserver
	## if inverse is already cached it return the cached value
	##  eg  cacheSolve(x)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
		data <-x$get()
		m <- solve(data, ...)
		x$setinverse(m)
		m
}
