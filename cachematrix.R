## The two functions below are used to create a special object that stores a matrix and chache its inverse.

## The makeCacheMatrix creates a special vector, which is a matrix, that contains functions to set and get the value of the matrix, then set and get the value of its inverse.

makeCacheMatrix <- function(m = matrix()) {
	v <- NULL
	set <- function(n) { 
		m <<- n
		v <<- NULL
	}
	get <- function() m 
	setinverse <- function(inverse) v <<- inverse 
	getinverse <- function() v 
	list(set = set, get = get, 
		setinverse = setinverse,
		getinverse = getinverse)
}

## The cacheSolve function computes the inverse of the matrix created with the makeCacheMatrix function above. First, it checks if the inverse has already been ccomputed. If it has, then the function gets the inverse from the cache and skips the calculation. Otherwise, it calculates the inverse of the matrix and sets the value in the cache via the setinverse function. 

cacheSolve <- function(m, ...) {
	v <- m$getinverse()
	if(!is.null(v)) {
		message("getting cached data")
		return(v)
	}
	data <- m$get()
	v <- solve(data, ...)
	m$setinverse(v)
	v ## Return a matrix that is the inverse of 'm'
}
