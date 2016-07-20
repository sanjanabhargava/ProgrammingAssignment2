## makeCacheMatrix function is used to initialize the value of inverse to zero
## Used to initialize and retrieve the value of the matrix
##Also returns the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	
	get <- function() x
	setinverse <- function(inverse) m <<- inverse
	getinverse <- function() m
	list(set=set , get=get , setinverse=setinverse, getinverse=getinverse)
}

## Used to determine whether a cached value of the inverse of the matrix exists

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
		if(!is.null(m)){
			message("Getting cached data")
			return(m)
		}
		data <- x$get()
		m <- solve(data,...)
		x$setinverse(m)
		m
}

