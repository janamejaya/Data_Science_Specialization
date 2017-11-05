## The following two functions demonstrate how lexical scoping
## can be used to create objects that cache the function and its
## environment

## This function gets and sets the matrix inverse in cache

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
           x <<- y
	   i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Given a square matrix (which is assumed to have an inverse matrix),
## this function calculates the inverse of the matrix on a new call,
## or uses a Cached copy of the matrix inverse for subsequent calls. 
## The first step is to check if a cached copy exists by checking
## if "i" is null or not. When null, the cached matrix is retrieved
## else the inverse matrix is calculated with the solve() function
cacheSolve <- function(x, ...) {
       i <- x$getinv()
       if(!is.null(i)){
          message(" getting cached matrix inverse data ")
	  return(i)
       }
	        
       matrix_data <- x$get()
       i <- solve(matrix_data,...)
       x$setinv(i)
       i
}
