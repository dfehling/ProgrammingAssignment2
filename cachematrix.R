## This script enables retreival of a cached inverse of a square invertible matrix and calculation and caching of the inverse if the inverse is not already cached

## Given an invertible matrix, 'x', this function will look up the stored inverse. If the inverse is not stored, the function will calculate it and store it

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i
	list(set = set, get = get, 
		setinverse = setinverse,
		getinverse = getinverse)
}


## This function returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- i$get()
        i <- solve(x)
        x$setinverse(i)
        i
}