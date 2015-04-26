# makeCacheMatrix creates a special "matrix", which is really 
# a list containing a function to

# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

##-------------------------------------------------

makeCacheMatrix <- function(x = matrix()) {
  
## initiating empty matrix "invm"  
  invm <- NULL
  
  set <- function(y) {
## substituting the value of x with y
    x <<- y
    
## reinstating the empty matrix
    invm <<- NULL
  }

  get <- function() x

## set the inverse matrix
  setinverse_m <- function(inverse) invm <<- inverse

## get the inverse matrix
  getinverse_m <- function() invm

  list(set=set, get=get, setinverse_m=setinverse_m, getinverse_m=getinverse_m)  

}


## This function, "cacheSolve", assumes that the inverse matrix is been computed.
## If the inverse matrix is not computed, it gets it from the cache in 
## makeCacheMatrix, computes the inverse of the special "matrix" and returns it.

cacheSolve <- function(x, ...) {
  
  invm <- x$getinverse()
  
  if(!is.null(invm)) {
    
    message("getting cached data.")
    
    return(invm)
  }
  
  data <- x$get()
  
  invm <- solve(data)
  
  x$setinverse(invm)
  
  invm
}

##--------------------------------
## testing

x <- rbind(c(1, 4), c(4, 1))
m <- makeCacheMatrix(x)
m$get()

## the first run has no cache
cacheSolve(m)

## the second run Rretrieves the cache 
cacheSolve(m)

## End-------------------------------
