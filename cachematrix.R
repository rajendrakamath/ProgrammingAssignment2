## The 'makeCacheMatrix' function provides the get/set routines and
## enables caching
## The 'cacheSolve' function actually constructs the logic to retrieve 
## from the cache (or calaculate)

## Create a function which enables caching and provides get/set routines

makeCacheMatrix <- function(x = matrix()) {

	##Start with an empty matrix
	m <- NULL

	## delcare another function set where the value will be cached in 
	## 1. Matrix is created for the first time. 
	## 2. changes made to cached matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
	}
	
	## gets the value of the inverse
	get <- function() x

	##sets the inverse matrix value
        setinverse <- function(inverse) m <<- inverse

	## get the inverse
        getinverse <- function() m

	## passes the value of the function makeCacheMatrix
        list(set = set, get = get,
      	setinverse = setinverse,
            getinverse = getinverse)
}


## This function contains the logic to return either the cached value or to 
## perform the actual inverse using the 'solve' function call

cacheSolve <- function(x, ...) {

	m <- x$getinverse()
      
	## if the value is already in the cache, return the cached value
	if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }

	## otherwise call 'solve' and calclute the inverse, set it and return
	message("calculate inverse and retrieve data")
	data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}


