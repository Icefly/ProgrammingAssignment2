## makeCacheMatrix function that generates a set of functions
## get the value
makeCacheMatrix <- function(x=matrix()){
  sol <- NULL  ## sol variable will be where the inverse matrix is saved.
  set <- function(y){
    x <<- y
    sol <<- NULL
  }
  get <- function()x
  setinverse <- function(solve) sol<<-solve  ## calculates the inverse matrix
  getinverse <- function() sol ## get the cached data
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}
  
cacheSolve <- function(x) {  
  ## x is the result of makeCacheMatrix
  ## for example, if you ran the makeCacheMatix; result<-makeCacheMatrix(matrix)
  ## then it will be cacheSolve(result)
  
  sol <- x$getinverse()  ## retrieve the inverse matrix value calculated in the 
                         ## prior function and save in sol
  if (!is.null(sol)) {   ## if sol has value, in other words, it was calculated before,                  
    message ("will get the cached result")
    return(sol)         ## use the prior calculation result
  }
  data = x$get()        ## if not previously calculated, get the matrix
  sol <- solve(data)    ## and calculate the inverse matrix
  x$setinverse(sol)
  sol
}
