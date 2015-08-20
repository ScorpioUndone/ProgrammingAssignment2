## makeCacheMatrix contains functions that set and get both the matrix input and its inverse

makeCacheMatrix <- function(x = matrix()) { 
    ##  takes 'x' as input matrix
    ##  creates an instance of object 'i' and sets it to NULL. The inverse will be
    ##  stored here later.
    i <- NULL   
    set <- function(y) {
        x <<- y
        i <<- NULL
        ##  since the matrix is assigned a new value here, flush the cache
        ##  super-assignment operator ensures that both 'x' and 'i' will be available
        ##  in other environments (i.e. in cacheSolve)
    }
    
    get <- function() x
    ##  set and retrieve the inverse stored in 'i'
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i          
    list (set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse )
}

## cacheSolve first looks for a cached inverse of matrix 'x', and if none exists, 
## will calculate one and set the value of the inverse in the cache via the setinverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## if data is stored in i, retrieve and return it
        i <- x$getinverse()
        if(!is.null(i)) {
            message("getting cached data")
            return(i)
        }
 
        ## gets matrix x and stores in variable data
        ## determines the inverse of 'data', which contains matrix 'x'
        ## stores inverse of 'x' as 'i'
        ## returns value of 'i'
        data <- x$get()
        i <-solve(data, ...)
        x$setinverse(i)
        i
}        
