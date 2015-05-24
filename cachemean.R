makevector <- function(x = numeric()) {
  
  ## creates a list of the functions that cachemean should use
  ## also caches the mean for cachemean
  ## v <-makevector(1:10) 
  ## v$get() should show 1 2 3... 10
  
  ## initialize m which is to cache mean to nothing
  m <- NULL
  
  ## the following functions will not run 
  ## untill called by others outside of this parent 
  ## function "makeVector"
  
  ## the set function
  ## set x to a new value stored in y
  ## reset mean to nothing
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## the get function
  ## return the value of x
  get <- function() x
  
  ## the setmean function
  ## set the variable "m" located outside of setmean 
  ## to a new value stored in argument "mean"
  ## m is defined in parent function makeVector
  setmean <- function(mean) m <<- mean
  
  ## the getmean function
  ## return the value of m
  getmean <- function() m
  
  ## returning a list of the functions defined above
  ## name of elements are set, get, setmean, getmean
  list(set = set, 
       get = get,
       setmean = setmean,
       getmean = getmean)
}


cachemean <- function(x, ...) {
  
  ##cachemean(v)
  
  ## get the cached mean m if possible
  m <- x$getmean()  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## get the cached data (varable called x inside makeVector)
  data <- x$get()
  
  ## calculate the latest value of mean
  m <- mean(data, ...)
  
  ## update cached mean
  x$setmean(m)
  
  ## return the latest value of mean
  m
  
}
