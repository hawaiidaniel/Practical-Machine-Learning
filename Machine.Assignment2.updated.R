# Caching the Inverse of a Matrix:
# Matrix inversion is usually a costly computation and there may be some benefit 
#to caching the inverse of a matrix rather than compute it repeatedly.
# The following two functions are used to create and stores a matrix and caches its inverse.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ## set() function to make the matrix passed in as the working matrix (x) 
  set <- function(y) {
    x <<- y
    #assign m to be NULL
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  #make a list of set, get, setinverse and getinverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}

#The cachSolve function returns the inverse of the matrix. First of all, it checks if the inverse is computed.
#If so, it shows the message "getting cached data" and skip the computation. If not, it computes the inverse and
#returns the value.
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  ## Return a matrix that inverse 'x'
}


# my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
# my_matrix$get()
# my_matrix$getinverse()
# cacheSolve(my_matrix)
# my_matrix$getinverse()
# my_matrix$set(matrix(c(3, 4, 5, 6), 2, 2))
# my_matrix$get()
# my_matrix$getinverse()
# cacheSolve(my_matrix)
# cacheSolve(my_matrix)
# my_matrix$getinverse()
