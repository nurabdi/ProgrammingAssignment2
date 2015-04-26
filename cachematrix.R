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

## set the matrix  
  set <- function(y) {
    
## substituting the value of x with y
    x <<- y
    
## reinstating the empty matrix
    invm <<- NULL
  }

## get the matrix
  get <- function() x

## set the inverse matrix
  setinverse_m <- function(inverse) invm <<- inverse

## get the inverse matrix
  getinverse_m <- function() invm

## list of the newly defined functions that we need to return

  list(set=set, get=get, setinverse_m=setinverse_m, getinverse_m=getinverse_m)  

}


## This function, "cacheSolve", assumes that the inverse matrix is been computed.
## So, if computed it returns. If not computed, it computes the inverse 
## of the special "matrix" and returns it.

cacheSolve <- function(x, ...) {
  
  invm <- x$getinverse()
  
## this function return the matrix if is already computed
 
  if(!is.null(invm)) {
    
    message("getting cached data.")
    
    return(invm)
  }
  
## the inverse is not computed so we compute it here
  data <- x$get()
  
  invm <- solve(data)

## inverse is cahed here
  
  x$setinverse(invm)

## the inverse matrix is returned
  
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
