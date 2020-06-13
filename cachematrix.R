makeCacheMatrix <- function(x = matrix()) {
  matrixinverse<-NULL
  set<-function(y){
    x<<-y
    matrixinverse <<-NULL
  }
  get<-function()x
  setinverse<-function(solve){ matrixinverse <<-solve}
  getinverse<-function()matrixinverse
  list(set=set,get=get,
       setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  matrixinverse <-x$getinverse()
  if(!is.null(matrixinverse)){
    message("getting cached data")
    return(matrixinverse)
  }
  datas<-x$get()
  matrixinverse<-solve(datas,...)
  x$setinverse(matrixinverse)
  matrixinverse
}
