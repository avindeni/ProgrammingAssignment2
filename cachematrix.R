## Together, the two functions create a special "matrix" list that stores a
## numerix matrix and caches its inverse.

## The first function creates functions which set and gets the value of the
## matrix to cache and then sets and gets the value of the inverse. This is
## followed by the creation of the special "matrix"

makeCacheMatrix <- function(x = matrix()) {

	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) m <<- solve
	getinverse <- function() m
	list(set = set, get = get, 
			setinverse = setinverse, 
			getinverse = getinverse)

}


## The second function checks that the passed matrix argument has not been
## inverted and the matrix has not changed. If not, it calculates the inverse.

cacheSolve <- function(x, ...) {
	m <- x$getinverse()
	if((!is.null(m)) & all((isTRUE(x != m)))) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m
}
