## Put comments here that give an overall description of what your
makeCacheMatrix <- function(x=matrix()){
  sol <- NULL
  set <- function(y){
    x <<- y
    sol <<- NULL
  }
  get <- function()x
  setreverse <- function(solve) sol<<-solve
  getreverse <- function() sol
  list(set=set, get=get, setreverse=setreverse, getreverse=getreverse)
}
  
cacheSolve <- function(x, mat=matrix()) {
  sol <- x$getreverse()
  if (!is.null(sol)) {                      
    message ("getting the chached data")
    return(sol)
  }
  data = x$get()
  sol <- solve(mat)
  x$setreverse(sol)
  sol
}
