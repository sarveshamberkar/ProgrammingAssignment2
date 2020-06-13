makeCacheMatrix <- function(x = matrix()) {
  matrixinverse<-NULL
  
  set<-function(y){
    x<<-y
    matrixinverse <<-NULL
    ## change the value of inverse of the matrix in case the matrix was changed.
  }
  
  get<-function()x
  #calculates the inverse of non-singular matrix via the solve function
  setinverse<-function(solve){ matrixinverse <<-solve}
   ## gets the value of the inverse
  getinverse<-function()matrixinverse
  list(set=set,get=get,
       setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  matrixinverse <-x$getinverse()
  #if the inverse exists, it gets it.
  if(!is.null(matrixinverse)){
    message("getting cached data")
    return(matrixinverse)
  }
  datas<-x$get()
  matrixinverse<-solve(datas,...)
  x$setinverse(matrixinverse)
  matrixinverse
}
