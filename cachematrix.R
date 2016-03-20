## These two functions calculate the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y) {
		x <<- y
		inverse <<- NULL
	}
	get <- function() x
	set_inverse <- function(inverse) i <<- inverse
	get_inverse <- function() i
	list(set = set, get = get,
		set_inverse = set_inverse,
		get_inverse = get_inverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	i <- x$get_inverse()
		if(!is.null(i)) {
			message("getting cached data")
			return(i)
		}
	data <- x$get()
	i <- solve(data, ...)
	x$set_inverse(i)
	i
}
