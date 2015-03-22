## Two functions are defined here, makeCacheMatrix & cacheSolve
## The purpose of these functions is to calculate the inverse of a matrix.  
## Note- rather than simply performing the inversion, they cache the result of the inversion.
## This means that if the same call is made again, the cached results is returned and avoids 
## having to compute the results again.
 
 
###   “makeCacheMatrix” creates a special "vector" a list containing a function to:
##   1) set the value of the vector
##   2) get the value of the vector
##   3) set the value of the inverse matrix
##   4) get the value of the inverse matrix
makeCacheMatrix <- function(x = numeric()) {
  m <- NULL              
  set <- function(y) {  
    x <<- y              
    m <<- NULL           
  }
  get <- function() {x}         
  setsolve <- function(solve)   
  m <<- solve                   
  getsolve <- function() {m}    
  list(set = set, get = get,     
       setsolve = setsolve,     
       getsolve = getsolve)     
}
 
 
### cacheSolve calculates the mean of the special "vector" which created in makeCacheMatrix
##    First checks to see if the mean has already been calculated. 
##    If so, it gets the mean from the cache and skips the computation. 
##    Otherwise, it calculates the mean of the data and sets the value of the mean in the cache via the setmean function.
cacheSolve <- function(x, ...) {
  m <- x$getsolve()   
  if(!is.null(m)) {   
    message("getting cached data")
    return(m)
  }
  data <- x$get()        
  m <- solve(data, ...)  
  x$setsolve(m)          
  m                      
}
