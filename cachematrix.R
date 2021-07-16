## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  invmat<- NULL
  setmat<- function(y) {
    x<<-y
    invmat<<-NULL
  }
  getmat<-function() {x}
  setinvmat<-function(inverse) {invmat<<-inverse}
  getinvmat<-function() {invmat}
  list(set=setmat,getmat=getmat, setinvmat=setinvmat, getinvmat=getinvmat)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  invmat<-x$getinvmat()
  if(!is.null(invmat)) {
    return(invmat)
  }
  mat<-x$getmat()
  invmat<-solve(mat,...)
  x$setinvmat(invmat)
  invmat
}
