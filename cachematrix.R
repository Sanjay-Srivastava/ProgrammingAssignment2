## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function does the following:
## 1 - declares a global variable 'm' of type matrix that can cache the inverse of matrix 'x' within the environment in
##     which 'x' is defined
## 2 - accepts an invertible matrix 'x'. If that matrix is not invertible, the code will error out
## 3 - defines 'set' function that can be used to accept a new matrix 'y', and reset 'x' = 'y' and inverse 'm' back to null 
## 4 - defines 'setinverse' that invokes the R 'solve' function to invert the matrix 'x' and store the result in matrix 'm'
## 5 - defines 'get' function to retrieve the value of matrix 'x'
## 6 - defines 'getinverse' function to retrieve the value stored in matrix 'm' 
## 7 - finally, attaches a list of these sub-functions to the object makeCacheMatrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
      x <<- y
      m <- NULL
      
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list (get = get, set = set, getinverse = getinverse, setinverse = setinverse)

}


## Write a short comment describing this function
## This function does the following:
## 1 - for a given invertible matrix 'x', invokes the getinverse function defined under makeCacheMarix to get the inverse
## 2 - retrieves from the cache, the inverse of 'x' by calling it's 'getinverse' function, and stores it in 'm' 
## 3 - if 'm' is non-null, i.e. it was cached, then, it prints a message signifying that the data was retrieved from the
##      cache, and no computation was performed, and then proceeds to print the inverse matrix 'm' to the console
## 4 - if 'm' was null, i.e. not cached, then, it invokes the get() method under the function makeCacheMatrix to fetch the
##      value of matrix 'x' into a variable called 'data. It then invokes the 'solve' R function to tabulate the inverse and
##      stores that into 'm'
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
