## Two following functions make an advantage of the R skoping rules to reduce the time of computing
## an inverse matrix. Matrix inversion is usually a costly computation and, therefore, there may be
## some benefit to use cached values. The first function can cache a matrix inverse, whereas
## the second computes the inverse of the matrix which was created by the first one. 

## This function creates a list containing a function to:
## 1. set the matrix
## 2. get the matrix
## 3. set the inverse matrix
## 4. get the inverse matrix

makeCacheMatrix <- function(x = matrix()){
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function () x
      setsolve <- function(solve) m <<- solve
      getsolve <- function () m
      list(set = set, get = get,
           setsolve = setsolve,
           getsolve = getsolve)
}

## This function calculates the inverse matrix created with the above function. 
## Firstly, it checks whether the inverse matrix has already been calculated.
## Then it retrieves the inverse matrix from the cache and skips the calculation printing a message.
## Otherwise, it inverts the matrix via solve() function and sets it with the setsolve function.

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