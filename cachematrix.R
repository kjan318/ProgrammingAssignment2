## Put comments here that give an overall description of what your
## functions do

## To creat makeCacheMatrix function for :
# set the value of the vector
# get the value of the vendor
# set the value of the inverse
# get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  j <- NULL
  set <- function(y){
    x <<- y
    j <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) j <<- inverse
  getInverse <- function() j 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## cacheSolve function is to compute the inverse of the special “matrix” 
# returned by makeCacheMatrix above. If the inverse has already been calculated 
#(and the matrix has not changed), then cacheSolve should retrieve the inverse 
# from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  j <- x$getInverse()
  if(!is.null(j)){    # check j whether the inverse has already been calculated and the matrix has not changed or not.
    message("To get cached data")
    return(j)         #return existing makeCacheMatrix
  }
  
  a_mat <- x$get()    
  j <- solve(a_mat,...)  #it calculates the inverse of the data by "solve" function
  x$setInverse(j)        # and sets the value of the and sets the value of the mean in the cache via the setInverse function.
  j
}
