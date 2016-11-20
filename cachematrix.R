## Caching a matrix by creating several nested functions that allow us to cache 
## its value and its inverse.

#This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
		m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
		
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


 ## Return a matrix that is the inverse of 'x' from the cache if it's there otherwise 
 ## compute the inverse and cache it before returing it
cacheSolve <- function(x, ...) {
       m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        mymatrix <- x$get()
        m <- solve(mymatrix,...)
        x$setinverse(m)
        m
}
