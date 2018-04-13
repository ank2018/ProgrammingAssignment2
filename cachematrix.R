## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix function creates a special "matrix" object
# sets the matrix value
# gets the matrix value
# sets the inverse matrix value using solve 
# gets the inverse matrix value

makeCacheMatrix <- function(x = matrix()) {
    minv <- NULL
    set <- function(y){
      x <<- y
      minv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) minv <<- solve
    getinv <- function() minv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve computes inverse of special "matrix" returned by makeCacheMatrix.
# if the inverse is already calculated then it retrieves inverse from the cache
# else it calculates the inverse value using solve

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  minv <- x$getinv()
  if (!is.null(minv)) {
    message("getting cached data")    
    return(minv)
  }
  data <- x$get()
  minv <- solve(data,...)
  x$setinv(minv)
  minv  
}
