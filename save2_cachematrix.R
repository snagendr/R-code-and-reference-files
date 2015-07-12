## makeCacheMatrix, like makeVector, is a function that creates a special matrix object and returns a list of 4 functions
## get: returns the matrix
## set: sets the value of the matrix to a different value
## setinv: saves the matrix inverse and assigns that value to an object (m_inv) in an environment that is different from the current environment
## getinv: returns the stored value of the inverse of matrix x

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

        m_inv <- matrix(data=NA)
        set <- function(y) {
                x <<- y
                m_inv <<- matrix(data=NA)
        }
        get <- function() x
        setinv <- function(inverse) m_inv <<- inverse
        getinv <- function() m_inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}
## Write a short comment describing this function

cacheSolve <- function(x, ...) {

### It calculates the inverse of the special "matrix" created with the above function. 
### This calculates the inverse of the matrix and returns the value. However, it first checks to see if the inverse has already been calculated by checking cache
### For this, it checks the first element of the inverse, and if it is NOT NA, then that means the inverse has already been calculated
### If so, it gets the matrix inverse from the cache and skips the computation
### Otherwise, it calculates the matrix inverse and sets the value of the inverse in the cache (higher environment) via the setinv function

       m_inv <- x$getinv()
       if(!is.na(m_inv[1,1])) {
           message("getting cached data")
           return(m_inv)
       }
       data <- x$get()
       m_inv <- solve(data)
       x$setinv(m_inv)
       m_inv
}