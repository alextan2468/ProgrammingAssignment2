## The makeCacheMatrix function will be used to create a matrix
## that can allow the storage of the matrix information itself
## as well as retrieving the matrix and the inverse of the matrix
## The inverse of the matrix would be solved by the cacheSolve function

## makeCacheMatrix creates a storage structure for a matrix
## with additional get function to retrieve matrix value
## and getinverse function to get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinversematrix <- function(inversematrix) m <<- inversematrix
  getinversematrix <- function() m
  list(set = set, get = get,
       setinversematrix = setinversematrix,
       getinversematrix = getinversematrix)
}


## this function can solve the inverse matrix of the object and save it 
## to the "cache" environment of the object created bymakeCacheMatrix
## if already solved before and stored, further calling will just return
## the inverse matrix stored in cache and no recalculation will be done

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinversematrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  datamatrix <- x$get()
  m <- solve(datamatrix) ##here the inverse matrix is solved
  x$setinversematrix(m)
  m
}
