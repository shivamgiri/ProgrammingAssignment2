## This program makes use of two functions makeCacheMatrix and cacheSolve 
## which is used to find inverse of an invertible matrix in an efficient manner
## by making use of Lexical scoping in R.

## The function 'makeCacheMatrix' makes use of <<- operator and is used to create a list
## A list containing functions to :
##              1. set the matrix
##              2. get the matrix
##              3. set the inverse of matrix
##              4. get the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {  ## x: a square invertible matrix
  
  set<-function(y){
    x<<- y                         ## using `<<-` to assign a value to an object in an environment 
                                   ## different from the current environment. 
    invrs<<-NULL
  }
  
  get<-function()x
  
  setinverse<-function(inverse) invrs<<-inverse
  
  getinverse<-function() invrs
  
  list(set=set, get=get,
       setinverse=setinverse,
       getinverse=getinverse)
  

}


## "cacheSolve" function computes the inverse of the "matrix" returned by 
## makeCacheMatrix(). If the inverse has already been calculated and the matrix
## has not changed, it will return the inverse from the cache directly.

cacheSolve <- function(x, ...) {
  invrs<-x$getinverse()
  
  if(!is.null(invrs)){           ## Check if the inverse has already been calculated
    print("Getting cached data") ## Provides satisfaction to the user
    return(invrs)                ## Return inverse matrix
  }
  
  mat<-x$get()                   ## In other case solving for inverse of matrix
  
  invrs<-solve(mat, ...)         ## solve function is used to calculate inverse of matrix
  
  x$setinverse(invrs)
  
  return(invrs)
                                 ## Return a matrix that is the inverse of 'x'
}
