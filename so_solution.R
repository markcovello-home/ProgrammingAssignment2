## {assignment root)/source/so_solution.R code to read/write a matrix from cache
##  Not sure I get most of this, but what is sure is we must:
##  1. Test to see if the matrix is in the solve cache
##  2. if matrix is not in cache, calculate inverse and cache it
##  3. If the matrix is in cache then just return it


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

# This function actually choose the source for th inverse and assigns it
# its parameter, x, will use the x from the cache if it's there
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