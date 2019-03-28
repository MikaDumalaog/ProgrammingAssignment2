

#This function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL 
  set <- function(y) { #this will set the inverse values from the vector
    x <<- y
    inv <-- NULL
  }
  get <- function() x #this will get the inverse values from the vector
  setInverse <- function(inverse) inv <<- inverse #this will set the inverse matrix 
  
  getInverse <- function() inv #this will get the inverse matrix created
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

#This function computes the inverse of the matrix returned by makeCacheMatrix above. 
#If the inverse has already been created, then it will simply retrieve the inverse from the cache. 

cacheSolve <- function(x, ...) {
  inv <- x$getInverse() #this checks to see if the inverse matrix has already been created
  if(!is.null(inv)) {
    message("getting cached data") 
    return(inv) 
  }
  
  #this gets the inverse from the cache once validated that it has already been created via the setInverse function   
  data <- x$get() 
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
#finally, this function will get the inverse matrix from the cache   
InverseMatrix <- makeCacheMatrix(matrix(1:4, 2, 2))
InverseMatrix$get()
InverseMatrix$getInverse()

cacheSolve(InverseMatrix) 