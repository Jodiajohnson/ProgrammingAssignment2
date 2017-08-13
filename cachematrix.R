## Creates a matrix that caches its inverse, then gets the inverse
## the second function checks to see if the inverse has been cached

## Creates a matrix and caches its inverse

makeCacheMatrix <- function(x = matrix(1:4, nrow = 2, ncol = 2)) {
	m <- NULL
	set <- function(y) {
		x <<- y
		s <<- NULL
	}
	get <- function() x
	setinverse <- function(solve)  s <<- inverse
	getinverse <- function () s
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)

}


## Checks to see if the inverse of previous function makeCacheMatrix
## has been cached, if not it obtains the inverse

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
	  if(!is.null(s) {
		message("getting cache data")
		return(s)
	  }
	  data <- x$get()
	  s <- solve(data, ...)
	  x$setinverse(s)
	  s

}
