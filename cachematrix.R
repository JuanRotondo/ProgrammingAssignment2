## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a matrix like object that can cache it's inverse

makeCacheMatrix <- function(x = matrix()) { ##argument is defined with default "matrix" value
	invMX <- NULL                           ## initialize invMX vector as NULL, holds value of matrix inverse
	set <- function(y) {					## Define set function to assign new value of matrix in parent env.
		x <<- y     						## reset invMX to NULL if there is a new matrix
		invMX <<- NULL
	}
	get <- function() x					## Define get function. Returns value of matrix argument

	setinverse <- function(inverse) invMX <<- inverse ## Assigns the value of invMX in parent environment
	getinverse <- function() invMX ## Gets the value of invMX
	list( set = set, get = get, setinverse = setinverse, getinverse = getinverse) # Name objects on list to be able te refer to them with $ operator

}


## Write a short comment describing this function
## Computes the inverse of the matrix like object from makeCacheMatrix
## If the inverse has been calculated (for the same matrix), then inverse is retrieved from cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invMX <- x$getinverse()
        if(!is.null(invMX)) {				## Checks wether inverse has been computed or not
        	message("getting cached data")
        	return(invMX)
        }
        data <- x$get()
        invMX <- solve(data, ...)			## Solves inverse of matrix
        x$setinverse(invMX)
        invMX
}
