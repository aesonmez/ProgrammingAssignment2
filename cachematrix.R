## makeCacheMatrix is a function that creates a special 
##              "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(xInv) m <<- xInv
  getInv <- function() m
  list(set      = set, get      = get,
       setInv = setInv,
       getInv = getInv)
  
}


##cacheSolve checks if there is a cached value and returns the value if 
##      available, otherwise recaculates the inverse and caches the new value
##cacheSolve function calculates the inverse of the special "matrix" created with the above function. 
##However, it first checks to see if the inverse has already been calculated. 
##If so, it gets the inverse from the cache and skips the computation. 
##Otherwise, it calculates the inverse of the matrix and 
##sets the value of the inverse in the cache via the setInv function

cacheSolve <- function(x, ...) {
# Return a matrix that is the inverse of 'x'
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached matrix...")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setInv(m)
  m
  
}
