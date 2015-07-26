## The following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list as a matrix containing a function to :
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
			x <<- y
			m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(
		set=set, 
		get=get, 
		setinverse=setinverse, 
		getinverse=getinverse
		)
}


# The following function returns the inverse of above matrix. First, it get the
# inverse of the matrix 'm'. If 'm' is not null, then it returns 'm' with 
# message "This is cache data". If not, it computes the inverse, sets the value 
# in the cache using setinverse function.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("This is cache data.")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m
}
