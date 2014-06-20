## Contains functions for storing and Retrieving Inverse of Matrix

## Stores the matrix and Caches Inverse
makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL;
  set<-function(y) {
    x <<- y;
    inv <<- NULL;    
  }
  
  
  get<-function() x;
  
  ##store the inverse matrix in Cache
  setCache <- function(invMatrix) {
    inv <<- invMatrix;
    
  }
  ## Returns the inverse of Matrix
  getInverseMatrix <- function() inv;
  
  ## Prepare a list with set and get methods
  list(set=set, get=get, setCache=setCache, getInverse=getInverseMatrix)  
  
}


## Returns the Inverse of Matrix, Stores in Cache if not present already 
cacheSolve <- function(x, ...) {
  
  orgMatrix <- x$get();  
  ## Check for Square Matrix and Invertible
  if(nrow(orgMatrix)==ncol(orgMatrix) && det(orgMatrix) !=0) {
    
    ## Check whether Cached inverse is available
    invMatrix <- x$getInverse();
    if(!is.null(invMatrix)) {
      message("Retreiving from Cache");    
      return(invMatrix);
    }
    
    invMatrix <- solve(orgMatrix);
    ##store inverse of the Matrix in Cache
    x$setCache(invMatrix);
    ## Return a matrix that is the inverse of 'x'
    invMatrix;
  }
  else {
    
    message("Not a valid Matrix for finding the inverse");
  }
  
}
