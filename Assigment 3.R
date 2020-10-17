# This function creates matrix object that can cache its inverse.

CacheMatrix <- function(sample = matrix()) {
  inv_sample <- NULL
  set <- function(x) {
    Sample <<- x
    inv_sample <<- NULL
  }
  get <- function() sample
  set_inverse <- function(inverse) inv <<- inverse
  get_inverse <- function() inv_sample
  list(set = set,
       get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}

# This computes the inverse of matrix created by CacheMatrix.
# If the inverse calculated and the matrix did not changed,
# then it should retrieve the inverse from the cache.

cacheSolve <- function(sample, ...) {
  ## Return the inverse of sample
  inv <- sample$get_inverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv_sample)
  }
  mat <- sample$get()
  inv_sample <- solve(mat, ...)
  sample$set_inverse(inv_sample)
  inv_sample
}