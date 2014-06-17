## Contains functions for storing and Retrieving Inverse of Matrix

## Stores the matrix and Caches Inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL;
  set<-function(y) {
    x<<-y;
    inv <<- NULL;
    
  }
  
  get<-function() x;
  
  setCache <- function(invMatrix) {
    inv <<- invMatrix;
    
  }
  
  getInverseMatrix <- function() inv;
  
  list(set=set,get=get,setCache=setCache,getInverse=getInverseMatrix)  
  
}


## Returns the Inverse of Matrix, Stores in Cache if not present already 
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  invMatrix <- x$getInverse();
  if(!is.null(invMatrix)) {
    message("Retreiving from Cache");    
    return(inv);
  }
  
  orgMatrix <- x$get();
  invMatrix <- solve(orgMatrix);
  x$setCache(invMatrix);
  invMatrix;
  
}
