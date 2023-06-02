## makeCacheMatrix:Caching the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  ##inverse property
inv<-NULL
##set Matrix
set<-function(y){
  x<<-y
  inv<<-NULL
}
##get Matrix x
get<-function()x
##return matrix x
x
##set Inverse Matrix
setInverse<-function(inverse)
  inv<<-inverse
##get Inverse Matrix
getInverse<-function()
  ##return Inverse
  inv
##return list
list(set=set,
     get=get,
     setInverse=setInverse,
     getInverse=getInverse)
}


## This function computes the inverse of the special "matrix" returned by
## makecacheMatrix above if the inverse has already been calculated (and the
##matrix has not changed), then the cachesolve should retrieve the inverse
##from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<-x$getInverse()
  ##return inverse if already set
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  ##Get Matrix
  data<-x$get()
  ##calculate the inverse using matrix multiplication
  inv<-solve(data)%% data
  ##Set Inverse
  x$setInverse(inv)
  ##Return Matrix
  inv
}
