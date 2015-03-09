## makeCacheMatrix function will create list of four member funtions set, get,
## setmatrix & getmatrix. 
makeCacheMatrix <- function(x = matrix()) {
  # we first assign NULL to mean so if first call is made to makeCacheMatrix
  # mean value will stay null so that new mean is calculated in cachesolve function
  m<-NULL
  
  # we use the "<<-" operator to set the value of x and m because we want 
  # to modify x and m defined in the enclosing environment (created 
  # when makeCacheMatrix was first called), not in the environment local to set(),
  # in which x and m are undefined.
  # we must reset m to NULL since we are modifying the underlying
  # vector and the cached value is no longer the valid 
  set<-function(y){ x<<-y; m<<-NULL}
  
  get<-function() x # return original input matrix as $get()
  setmatrix<-function(solve) m<<- solve #set setmatrix function to solve the value of 
  # x ie to get matrix "x" inversed.  
  
  getmatrix<-function() m #this functions returns "m" which is inversed matrix
  
  # list will be returned by the following code as list of functions, which we can use
  # makeCacheMatrix object like 
  # x <- makeCacheMatrix(tempmatrix)
  # x$set(newmatrix) # to set current values from newmatrix
  # x$get # to copy contents of set matrix to matrix to be inversed
  # x$setmatrix # to set the inversed matrix
  # x$getmatrix will store the inversed matrix to be used across the environments 
  # to avoid recalculation
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}
# CacheSolve takes the caching matrix created by getmatrix  
cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix() # store the value of inversed matrix in m
  # if inverse is to be calculated for the first time m will adopt value "Null"
  # set by first statement in makeCacheMatrix or it will hold the inverse matrix if 
  # inverse was ever calculated on same matrix before the current instance
  if(!is.null(m)){
    message("getting cached data")
    return(m) # return previously calculated inverse matrix. We have to explicitly
    # ask to return value as m will always store the value and without return 
    # function will entire infinite loop
  }
  matrix<-x$get() # call get to apply values from matrix being passed for inverting
  m<-solve(matrix, ...)  # calculate inverse of the passed matrix 
  x$setmatrix(m) # set the inverse of passed matrix in x which is equivalent to
  # being cached and we dont have to recompute it
  
  m # return the cached matrix
}
