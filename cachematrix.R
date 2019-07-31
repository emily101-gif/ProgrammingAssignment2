## Put comments here that give an overall description of what your
## functions do

## this function will get a matrix and set the value of the matrix and also set the
#inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) {
        matrixinv <- NULL
        setMatrix <- function(y) {
                x <<- y
                matrixinv <<- NULL
        }
        getMatrix <- function() x  # this gets the value of the matrix
        setInverse <- function(inverse) matrixinv <<- inverse # this sets the value of the inverse matrix
        getInverse <- function() matrixinv # this gets the value of the inversible matrix 
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setInverse = setInverse,
             getInverse = getInverse)
        
}


## this next function takes the outupt of the preious function and checks if it has an inverse matrix
#then it checks the inverse matrix has a value or not
#if the inverse matrix is empty then it sets it using the solve function from the original data
#if it inversse matrix does have a value then it returns the cached object 

cacheSolve <- function(x, ...) {
        matrixinv <- x$getInverse()
        if(!is.null(matrixinv)) {
                message("getting cached data")
                return(matrixinv)
        }
        data <- x$getMatrix()
        matrixinv <- solve(data, ...)
        x$setInverse(matrixinv)
        return(matrixinv)
}

## Return a matrix that is the inverse of 'x'

#testing it 
TestMatrix <- matrix(1:4, 2, 2)
CacheMatrix <- makeCacheMatrix(TestMatrix)
CacheMatrix$getMatrix()
CacheMatrix$getInverse()
cacheSolve(CacheMatrix)
