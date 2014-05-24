## makeCacheMatrix and cacheSolve cache the inverse of a matrix x.  
## So, typing MX <- makeCacheMatrix(x) will set and store a matrix, 
## and cacheSolve(x) will solve the inverse (and that inverse will be stored in a).
## As long as the matrix in MX doesn't change, the inverse cacheSolve will 
## return the cached value of that inverse.

## makeCacheMatrix sets and stores the value of the matrix, and outputs a list
## including the subfuntions, their values and environments.  The matrix can be changed either
## by calling makeCacheMatrix again, or by MX$set(y), where y is the new matrix.

makeCacheMatrix <- function(x = matrix()) {
	m <- matrix(nrow = dim(x)[1], ncol = dim(x)[2]) ##creates matrix of NAs with dimensions of x
	set <- function(y){
		x <<- y
		m <<- matrix(nrow = dim(x)[1], ncol = dim(x)[2])
	}
	get <- function() x
	setinverse <- function(solve) m <<- solve
	getinverse <- function() m
	list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve calculates the inverse of the matrix set in makeCacheMatrix.
## If no changes have been made to the matrix set in makeCacheMatrix, then 
## cacheSolve will return the cached value (and not recalculate the inverse).

cacheSolve <- function(x, ...) {
	m <- x$getinverse()     
	if(!is.na(m[1,1])) {  ##if no changes were made to x, then...
		message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...) ##calculates inverse of data
    x$setinverse(m) ##assigns the non NA value to m in setinverse
    m
    }