## The following functions "makeCacheMatrix" is used to make the cache matrix and "cacheSolve" is used to solve its inverse.

## "makeCacheMatrix" firstly sets the value of matrix, then gets the value of matrix, 
## it then sets the value of the inverse of the matrix and then gets the value of the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) 
{   
    inv <- NULL
    set <- function(y) 
    {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, 
         get=get, 
         setinverse=setinverse, 
         getinverse=getinverse)
}



## Compute the inverse of the special matrix returned by "makeCacheMatrix" above. 
## If the inverse has already been calculated (and the matrix has not changed),
## then the "cacheSolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) 
{
    inv <- x$getinverse()
    if(!is.null(inv)) 
    {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
