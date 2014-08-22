# makeCacheMatrix function creates actually a list of functions which allow to 
# set the value of matrix
# get the value of matrix
# set the inverse matrix
# get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}

# cacheSolve calculates the inverse of matrix created within makeCacheMatrix
# However, it first checks to see if the inverse matrix has already been calculated. 
# If so, it gets the inverse matrix from the cache and skips the computation. 
# Otherwise, it calculates the inverse matrix and sets the value of the inverse matrix 
# in the cache via the setmatrix function

cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}