## Put comments here that give an overall description of what your
## functions do

## Puts the value of the inverse of matrix if already calculated into the cache for the faster access.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y){
		x<<-y
		m<<-NULL
	}

	get <- function() x
	setmat <- function(solve) m <<- solve
	getmat <- function() m
	list(set=set, get=get, setmat=setmat, getmat=getmat)
}


## Returns the inverse of the matrix either by calculating it (if m i NULL) or taking it from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getmat()
	if(!is.null(m)){
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setmat(m)
	m
}
