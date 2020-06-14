makeCacheMatrix <- function(x = matrix()) {
  matrixinverse<-NULL
  ##declaring the value
  set<-function(y){
    x<<-y
    matrixinverse <<-NULL
    ## change the value of inverse of the matrix in case the matrix was changed.
  }
   ## Method the get the matrix
  get<-function()x
  ##returns the matrix
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
   ## Get the matrix from our object
  datas<-x$get()
  ## Calculate the inverse using matrix multiplication
  matrixinverse<-solve(datas,...)
   ## Set the inverse to the object
  x$setinverse(matrixinverse)
  ##return inverse of the matrix
  matrixinverse
}

##output1:
##source('cachematrix.R')
##mat<-matrix(c(1,4,9,0,-3,2,2,7,8),3,3)
## m1<-makeCacheMatrix(mat)
##cacheSolve(m1)

##    [,1]    [,2]     [,3]
##[1,] -1.18750  0.1250  0.18750
##[2,]  0.96875 -0.3125  0.03125
##[3,]  1.09375 -0.0625 -0.09375

##output 2:
##source('cachematrix.R')
##> mat<-matrix(1:4,2,2)
##> m1<-makeCacheMatrix(mat)
##> cacheSolve(m1)
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
