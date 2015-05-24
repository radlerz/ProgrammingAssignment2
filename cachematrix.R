##
## Matrix inversion is usually a costly computation and there may be 
## some benefit to caching the inverse of a matrix rather than compute 
## it repeatedly.  The following pair of functions are to achieve this
## cache effect.
##
## makeCacheMatrix()
## This function creates a special "matrix" object that can cache its 
## inverse.  It provides both the data structure and the functions to
## manipulate this special "matrix" object.
## 
## cacheSolve() 
## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above.  If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.
##

##
## makeCacheMatrix()
## Purpose: define data structure and functions for caching a matrix
## Input:   optionally takes in a matrix and stores it
## Output:  returns a list of functions for manipulating the matrix stored
##
makeCacheMatrix <- function(aMatrix = matrix()) {
  
  ## the following code run once when the function is being created
  ## there is no invertedMatrix to begin with
  invertedMatrix <- NULL
  
  ##
  ## the following code will not run untill called by others
  ## outside of this function.  For example:
  ##
  ## cacheMatrix <- makeCacheMatrix(newMatrix)
  ## cacheMatrix$setMatrix(newMatrix)
  ## cacheMatrix$getMatrix()
  ##
  
  ## set the stored matrix to a new matrix being passed in as argument
  setMatrix <- function(newMatrix) {
    aMatrix <<- newMatrix
    invertedMatrix <<- NULL  ## original matrix changed, reset inverse
  }
  
  ## return the stored matrix
  getMatrix <- function() {
    aMatrix
  }
  
  ## set the stored matrix to a new matrix being passed in as argument
  setInverse <- function(anInvertedMatrix) {
    invertedMatrix <<- anInvertedMatrix
  }
  
  ## return the stored matrix
  getInverse <- function() {
    invertedMatrix
  }
  
  ## returning a list of the functions defined above
  list(setMatrix = setMatrix, 
       getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)

}

##
## cacheSolve()
## Purpose: return the inverse of a matrix from the cache if it exists,
##          otherwise compute the inverse of the matrix, store the result 
##          in the cache before returning the result
## Input:   an object created by the makeCacheMatrix() function
## Output:  returns the inversed form of the matrix given
## Assumes: matix supplied is always invertible
##
cacheSolve <- function(cacheMatrix) {
  
  ## get the cached inverse
  invertedMatrix <- cacheMatrix$getInverse()  
  
  ## check if cache exists already
  if(is.null(invertedMatrix)) {
    
    ## cache of inverse does not exist yet
    
    ## get the original matrix
    originalMatrix <- cacheMatrix$getMatrix()
      
    ## calculate the inverse
    invertedMatrix <- solve(originalMatrix)
    
    ## update cached inverse
    cacheMatrix$setInverse(invertedMatrix)
    
  } else {
    
    message("getting cached invserse of the matrix")
  
  }
  
  ## return the inverse
  invertedMatrix
  
}

##
## testCases()
## Purpose:  to define and run test cases without typing repeatedly
##
testCases <- function() {
  
  # initialize functions and data structures
  cm <-makeCacheMatrix()
  
  # set the original matrix 
  cm$setMatrix(matrix(1:4, 2, 2))
  
  # show the original matrix
  m <- cm$getMatrix()
  print(m)
  
  # compute inverse for the first time
  i <- cacheSolve(cm)
  print(i)
  
  # the message saying inverse is got from cache should be printed this time  
  i <- cacheSolve(cm)
  print(i)
  
}

##
## end of R script cachematrix.R
##