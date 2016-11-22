## There are basically 2 functions in this program and these functions are used for lexical scoping and ptimizing the complex operations

## makeCacheMatrix function helps in finding the inverse and it has the function to set get the matrix and to set and the inverse values of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) m <<- inverse
  getinv <- function() m
  list(set = set, 
       get = get,
       setinv = setinv,
       getinv = getinv)

}


## CacheSolve function helps in finding the inverse and if the value is already found it will get the cached value

cacheSolve <- function(x, ...) {
       
    m <- x$getinv()
    if(!is.null(m))
    {
      message("getting inverse matrix from cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
    
    ## Return a matrix that is the inverse of 'x'
}
