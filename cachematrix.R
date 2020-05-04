##  BY:     Jerome Cordjotse
##  TITLE:  Programming Assignment 2

##  CACHING THE INVERSE OF A MATRIX                                                                             
# Caching is storing data to make faster future requests 
# The functions below help to work with cached matrix inverse to
# prevent recomputation of inverse of the same matrix

## makeCacheMatrix
## This function creates a lists of setters and getters functions
## to set and retrieve respectively the matrix and it's inverse

makeCacheMatrix <- function(x = matrix()) {
  ## i for the inverse variable
    i <- NULL
  print(i)
  ## SETTERS: set values to global variables x, i
    set <- function(y){
        x <<- y     #Assignment to variable outside this() scope
        i <<- NULL  #Reintiialisation to NULL when matrix changes 
    }
    setinverse <- function(inverse){ i <<- inverse}
  
  ## GETTERS: retrieve global variables x, i
    get <- function() {x}
    getinverse <- function() {i}
  
  # output of a list of functions of setter and getters above
    list(
      set = set, 
      setinverse = setinverse,
      get = get, 
      getinverse = getinverse
         )
}


## cacheSolve
## This function modifies function solve() to return a cached variant if it exists

cacheSolve <- function(x, ...) {  ## Return a matrix that is the inverse of 'x'

    i <- x$getinverse()                   ## Check if inverse has been 
    if(!is.null(i)) {                     ## cached and return cached value
        message("retrieving available cahced data...")
        return(i)
    }
    ## If no cached inverse value exists, solve for inverse
    i <- solve(x$get(), ...)
    x$setinverse(i)
    i
}

## Copyright (C) 2020 Jerome Cordjotse - All Rights Reserved
## Thank You!