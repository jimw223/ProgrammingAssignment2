## These functions compute and cache the inverse inverse of an
## invertible matrix. Caching is more efficient when the same
## matrix must be inverted more than once.

## makeCacheMatrix sets an invertable matrix, get the matrix, solves and caches
## its inverse, and gets the cached inverse instead of recalculating it. 


makeCacheMatrix <- function(x = matrix()) {
  inverseX<-NULL
  set<-function(y) {
    x<<-y
    inverseX<<-NULL
  }
  get<-function() x
  setInverse<-function(solve) inverseX <<- solve
  getInverse<-function() inverseX
  list( set = set, get = get,
        setInverse = setInverse, 
        getInverse = getInverse 
  )
}

## cacheSolve checks for x's inverse in the cache and returns it if
## it exists. If it doesn't, the inverse is calculated and cached for future use.

cacheSolve <- function(x, ...) {
  
  inverse<-x$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data<-x$get()
  inverse<-solve(data,...)
  x$setInverse(inverse)
  inverse
}


