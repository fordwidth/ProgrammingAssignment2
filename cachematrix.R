#collective funcion called cachematrix.R
#1. > aMatrix <- makeCacheMatrix(matrix(1:4, 2, 2)) #stores matrix in parent environment
#2. > cacheSolve(aMatrix)   # gives matrix inverted 
#3. > cacheSolve(aMatrix)   # gives cached data of previous run 
## 
## makeCacheMatrix stores matrix in parent environment
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL                       ##initialize inv
    set <- function(y) {
        x <<- y                       ##stores given matrix x in parent environ 
        inv <<- NULL                  ##stores inv in parent environ
    }
    get <- function() x               ## retrieves x from parent env
    setinverse <- function(inverse) inv <<- inverse 
    getinverse <- function() inv      ##defines getter for the inverse inv
    list(set = set,                   ## gives name 'set' to set() function defined above
         get = get,                   ## gives name 'get' to get() function defined above
         setinverse = setinverse,     ## gives name 'setmean' to setmean() function defined above
         getinverse = getinverse)     ## gives name 'getmean' to getmean() function defined above
}

## Prints the inverse of a given matrix but calls up cached data if already there 
## to same time spent calculating new one
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()             ## Return a matrix that is the inverse of 'x'
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()                   ## get matrix x
    inv <- solve(data, ...)           ## solve or "invert" x matrix to inv
    x$setinverse(inv)                 ## I don't entirely understand this command
    inv                               ## return the inverted matrix 
}
