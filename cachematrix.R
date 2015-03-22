#The function is used to cached the inverse of a matrix


# The makeCacheMatrix is used to create a special matrix 
# object that can cahe its inverse.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    setmatrix <- function(solve) m <<- solve
    getmatrix <- function() m
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
}

# cacheSolve computes the inverse of a matrix. If it has already 
# been computed, its is retrieved from the cache.
cacheSolve <- function(x=matrix(), ...) {
    m <- x$getmatrix()
    if(!is.null(m)) {
        message("retrieving cached data")
        return(m)
    }
    
    matrix <- x$get()
    m <- solve(matrix, ...)
    x$setmatrix(m)
    m
}
