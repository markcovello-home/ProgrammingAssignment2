## {assignment root)/source/so_solution.R code to read/write a matrix from cache
##  Not sure I get most of this, but what is sure is we must:
##  1. Test to see if the matrix is in the solve cache
##  2. if matrix is not in cache, calculate inverse and cache it
##  3. If the matrix is in cache then just return it
## two functions accomplish this makeCacheMatrix() makes a, "special," matrix 
## that chacheSolve() can use to either calculate or extract from cache 
## the inverse of the matrix that was the argument originally passed to makeCacheMatrix()

## makeCacheMatrix() takes an invertible matrix and returns a, "special matrix," 
## (in fact it's  a list and it looks a lot like a list of instructions)
## that chacheSolve()  can use to return the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) { 
  ## initialize the value of the inverse to NULL,
  ## matrixinverse is the token to identify the inverse (solution) in this function
  matrixinverse <- NULL                     
  ## This simple function just puts it's argument into the cache(parent env)
  ## th token that holds the cached object in the parent env is x
   set <- function(y) {                      
    x <<- y
    ## 
    matrixinverse <<- NULL              
  }
  ## gets the value of the inverse; x is the token foe the cached inverse
  get <- function() x                           
  #calculates the inverse of non-singular matrix via the solve function
  setinverse <- function(solve) matrixinverse <<- solve 
  # gets the inverse     
  getinverse <- function() matrixinverse        
  ## passes the value of the function makeCacheMatrix        
  list(set = set, get = get,                    
       setinverse = setinverse,
       getinverse = getinverse)
}

## chacheSolve() takes the special matrix from makeCacheMatrix() and returns either the 
## previously calculated cached inverse of the original input matrix 
## or calculates its inverse directly

cacheSolve<- function(x, ...) {  
 ## load the cache into matrixinverse             
  matrixinverse <- x$getinverse()
  #if the cache is not null just return it and we're done
  if(!is.null(matrixinverse)) {                 
    message("getting cached data - Inverse of the matrix")
    return(matrixinverse)
  }
  #otherwise , calculate the inverse and send it back with a message saying it was calculated.
  data <- x$get()   
  message("not cached - Calculating Inverse")                            
  matrixinverse <- solve(data, ...)
  x$setinverse(matrixinverse)
  matrixinverse
}
