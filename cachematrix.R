

#THis function accepts a matrix, and returns a new object 
#that allows caching

makeCacheMatrix <- function(x = matrix()) {
m<- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get<-function()x
  setInv <- function(inverse) m <<- inverse
  getInv <- function() m
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)



}

#This is the workhorse function. It grabs the matrix,and calculates the inverse of that matrix
cacheSolve <- function(x, ...) {
       
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInv(m)
# Returns the inverse of matrix
  m
}