## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  ## inv is inverse of matrix x
  inv<-NULL
  
  ##set function sets x matrix to value of y matrix
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  ## get function returns x matrix
  get<-function() x
  
  setinverse<-function(inv_matrix)inv<<-inv
  getinverse<-function() inv
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
  
}


## cacheSolve: This function computes the inverse of the special
## "matrix" returned by makeCacheMatrix above. If the inverse has
## already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv<-x$getinverse()
  if(!is.null(inv)){
    message("getting cached inverse")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data,...)
  x$setinvers(inv)
  inv
}
