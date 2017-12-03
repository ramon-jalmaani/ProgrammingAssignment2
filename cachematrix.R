## Caching the Inverse of a Matrix: 

## Matrix inversion is usually a costly computation 
## and there may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly.
## Below are a pair of functions used to create a special object
## that stores a matrix and caches its inverse.

## This first function builds a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  	inv <- NULL
  	set <- function(y) {
    		x <<- y
    		inv <<- NULL
  	}
  	get <- function() x
  	setinverse <- function(inverse) inv <<- inverse
  	getinverse <- function() inv
  	list(set = set, 
       	     get = get,
       	     setinverse = setinverse,
       	     getinverse = getinverse)
}


## This second function calculates the inverse of the special "matrix"
## created by makeCacheMatrix above.
## If the inverse has been computed already and the matrix has not changed,
## then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'  
	inv <- x$getinverse()
  	if(!is.null(inv)) {
    		message("getting cached data")
    		return(inv)
  	}
  	data <- x$get()
  	inv <- solve(data, ...)
  	x$setinverse(inv)
  	inv
}
