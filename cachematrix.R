## Put comments here that give an overall description of what your
## functions do

## Creates a "CacheMatrix" object that is capable of returning inverse matrix.
makeCacheMatrix <- function(x = matrix()) {
  cachedInverse <- NULL
  
  ## Returns original Matrix
  get <- function() {
    return(x)
  }
  
  ## Resets object to new Matrix
  set <- function(y) {
    x <<- y
    cachedInverse <<- NULL
  }
  
  ## Inverse is calculated only once, cached for next request.
  getInverse <- function() {
    # If not cached, calculate and cache it for next time use
    if(is.null(cachedInverse)) {
      message("Solving and caching")
      cachedInverse <<- solve(x)
    } else {
      message("Getting cached inverse")
    }
    
    return(cachedInverse)
  }
  
  list(get = get, set = set, 
       getInverse = getInverse)
}


## Returns the inverse matrix of "CacheMatrix" object. 
## Computes if required otherwise returns cached solution.
cacheSolve <- function(x, ...) {
  ## Note that getInverse handles checking cache and calculating inverse if req
  return(x$getInverse())
}

###
## Testing
# source("cacheMatrix.R")
# a1 <- c(3, 2, 5)
# a2 <- c(2, 3, 2)
# a3 <- c(5, 2, 4)
# A <- rbind(a1, a2, a3)
# print(A)
#
# cm <- makeCacheMatrix(A)
# print(cacheSolve(cm))
# ### Expect to see "Solving and caching"
# print(cacheSolve(cm))
# ### Expect to see "Getting cached inverse"
###

