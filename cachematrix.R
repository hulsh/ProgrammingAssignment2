makeCacheMatrix <- function(x=matrix(,nrow=2,ncol=2)) {
  result<-matrix(,nrow=2,ncol=2) #set 2x2 matrix of NA's
  set <- function(y) { #create set fxn
    x <<- y
    result<<-matrix(,nrow=2,ncol=2) #set 2x2 matrix of NA's
  }
  get<-function()x
  setinverse <- function(solve)result<<-solve #create inv fxn
  getinverse<-function()result     
  list(set=set, get=get,        #create call ID's
       setinverse=setinverse, 
       getinverse=getinverse)
}
  
cacheSolve<-function(x,...) {
  result <-x$getinverse() 
  if(all(is.na(result))==F) {       #chk to see if inv already cached
    message("getting cached data")
    return(result)
  }
  data<-x$get()
  result<-solve(data,...)  #find inverse of matrix
  x$setinverse(result)
  result                   #print inverse of provided matrix
}






> # A simple matrix m1 with a simple matrix inverse n1
> # Define
> m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
> m1
      [,1]  [,2]
[1,]  0.50 -1.00
[2,] -0.25  0.75
> 

> # is the 2 row by 2 column Identity matrix I2
> I2 <- matrix(c(1,0,0,1), nrow = 2, ncol = 2)
> I2
     [,1] [,2]
[1,]    1    0
[2,]    0    1
> 
> # And so (by linear algebra) n1 %*% m1 is also equal I2.
> # (If n1 is the inverse of m1 then m1 is the inverse of n1.)
> # With m1 defined as above, n1 ( the inverse of m1) is
> n1 <- matrix(c(6,2,8,4), nrow = 2, ncol = 2)
> n1
     [,1] [,2]
[1,]    6    8
[2,]    2    4
> 
> # Checks:
> m1 %*% n1
     [,1] [,2]
[1,]    1    0
[2,]    0    1
> 
> n1 %*% m1
     [,1] [,2]
[1,]    1    0
[2,]    0    1
> 
> solve(m1)
     [,1] [,2]
[1,]    6    8
[2,]    2    4
> 
> solve(n1)
      [,1]  [,2]
[1,]  0.50 -1.00
[2,] -0.25  0.75
> 
> # So if you have programmed your functions 
> # correctly (in the file cachematrix.R),
> # (that, and your comments-explanation of how they work
> # are what you are graded on)
> # and sourced cachematrix.R so they are 
> # available in your R session workspace, then doing 
> #
> myMatrix_object <- makeCacheMatrix(m1)
> 
> # and then
> # cacheSolve(myMatrix_object)
> 
> # should return exactly the matrix n1
> cacheSolve(myMatrix_object)
     [,1] [,2]
[1,]    6    8
[2,]    2    4
> 
> # calling cacheSolve again should retrieve (not recalculate)
> # n1
> cacheSolve(myMatrix_object)
getting cached data
     [,1] [,2]
[1,]    6    8
[2,]    2    4
> 
> # you can use the set function to "put in" a new matrix.
> # For example n2
> n2 <- matrix(c(5/8, -1/8, -7/8, 3/8), nrow = 2, ncol = 2)
> myMatrix_object$set(n2)
> # and obtain its matrix inverse by
> cacheSolve(myMatrix_object)
     [,1] [,2]
[1,]    3    7
[2,]    1    5
> 
> cacheSolve(myMatrix_object)
getting cached data
     [,1] [,2]
[1,]    3    7
[2,]    1    5
> 
  
  
  #m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
  #n1 <- matrix(c(6,2,8,4), nrow = 2, ncol = 2)
  #myMatrix_object <- makeCacheMatrix(m1)
  #myMatrix_object <- makeCacheMatrix(n1)
  #cacheSolve(myMatrix_object)
  