## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) 
{
  i <- NULL
  set <- function(y)
  {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(solve) i <<- solve
  getinv <- function() i
  list(set = set,get = get,setinv = setinv,getinv = getinv)
}
# Calculates the inverse of the special "matrix" created with the above function. 
# However, it first checks to see if the inverse has already been calculated.
# If so, it gets the inverse from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the data and sets
# the value of the inverse in the cache via the setinv function.


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i))
  {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data,...)
  x$setinv(i)
  i
}
# Example
# mat<-matrix(1:4,2,2)#stores list in mat
# makeCacheMatrix(mat)#prints inverse of mat
#     [,1]  [,2]
#[1,] -2    1.5
#[2,] 1     -0.5