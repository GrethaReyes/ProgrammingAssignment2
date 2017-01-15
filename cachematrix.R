## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {	## STEP1 Initialialize the Objects (x and m) .... Please See https://github.com/lgreski/datasciencectacontent/blob/master/markdown/rprog-breakingDownMakeVector.md for details
  m <- NULL
  set <- function (y) { 			## STEP 2 defining the function of my setter since my object "x" is still missing and inaccessible
    x <<- y 				## the <<- means that I am setting "x" as an object in the parent variable
    m <<- NULL 				## m I think where my R would store my new computations after I have changed whatever is the content of x that is why it is set as NULL
  }
  
  get <- function () x			## I am now definining the function of my getter for the matrix x
  SetInvMatrix <- function(solve) m <<- solve ## I now defing the setter for the m
  GetInvMatrix <- function() m		## Gets the value of m
  ## STEP 3 CREATING A NEW OBJECT BY RETURNING A LIST()
  list(set = set,          		## gives the name 'set' to the set() function defined above
       get = get,          			## gives the name 'get' to the get() function defined above
       SetInvMatrix = SetInvMatrix,  		##gives the name 'SetInvMatrix' to the SetInvMatrix() function defined above
       GetInvMatrix = GetInvMatrix) 		## gives the name 'GetInvMatrix' to the GetInvMatrix() function defined above
  
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
  m <- x$GetInvMatrix()
  if(!is.null(m)) {
    message("getting cached data on inverse matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$SetInvMatrix (m)
  m					## Return a matrix that is the inverse of 'x'
}
