## Put comments here that give an overall description of what your
## functions do

## makes a list of functions

makeCacheMatrix <- function(x = matrix()) {
  
  m<- NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<- function() x
  setinv <- function(inv) m<<-inv
  getinv <- function() m
  # returning list
  list(get = get,set = set,getinv = getinv,setinv = setinv)

}


## solves for inverse if value is changed, 
##else gets from cachematrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m<-x$getinv()
  if (!is.null(m) ){
    message("getting cached data")
    return( m)
  }
  data <- x$get()
  m<-solve(data, ...)
  x$setinv(m)
  # returning inverse
  m
}
