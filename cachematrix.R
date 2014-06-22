## These functions allow for the creation of matrices that have
## lazily evaluated (and cached) inverses for performance

## This function creates a cached matrix from the specified input matrix
makeCacheMatrix <- function(x = matrix()) {
	## Internal variable to hold cached inverse
	i <- NULL

	## Function to set the matrix
	set <- function(y) {
		x <<- y
		i <<- NULL
	}

	## Function to return (get) the matrix
	get <- function() x

	## Function to set the (cached) inverse matrix
	setinverse <- function(inverse) i <<- inverse

	## Function to get the (cached) inverse matrix
	getinverse <- function() i

	## Return the list of internal functions
	list(set = set,
	     get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## This function solves for the inverse of the input matrix, caches it and return it
cacheSolve <- function(x, ...) {
	## Get the currently cached inverse
	i <- x$getinverse()

	## If the cached value is not null (i.e. we've already calculated and cached the inverse), return it
	if(!is.null(i)) {
		message("getting cached inverse")
		return(i)
	}

	## Otherwise, calculate the inverse, cache it, and return it
	data <- x$get()		## Get data
	i <- solve(data)		## Find inverse
	x$setinverse(i)		## Cache inverse
	i				## Return inverse
}
