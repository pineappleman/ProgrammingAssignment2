## The makeCacheMatrix and cacheSolve functions work in conjunction with each
## other to define a matrix and then return its inverse. makeCacheMatrix assigns
## x and m in the global environment as a function of y and defines the set, get,
## setinverse, and getinverse. cacheSolve uses the matrix created in makeCacheMatrix
## and determines the value of m. If m is already cached then the function will 
## print "getting cached data" as well as return m which is defined as the inverse
## of the function that returns the matrix from the makeCacheMatrix function.

## The makeCacheMatrix function creates a special matrix when the variable x is 
## defined and provides methods for cacheSolve to get and set the matrix.

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


## cacheSolve uses the components of the function makeCacheMatrix and returns
## the inverse of the matrix.

cacheSolve <- function(x, ...) {
	m <- x$getinverse()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m

}
