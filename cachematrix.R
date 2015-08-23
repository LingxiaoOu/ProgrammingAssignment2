makeMatrix <- function(x = numeric()){
    i <- NULL
    
    #set the value of the matrix
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    
    #get the value of the matrix
    get <- function()x
    
    #set the value of the inverse
    setinverse <- function(inverse) i <<- inverse
    
    #get the value of the inverse
    getinverse <- function()i
    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

cacheSolve <- function(x, ...){
    
    #get the value of the pre-calculated inverse (if any) of the matrix
    i <- x$getinverse()
    
    #if the inverse of the matrix has been pre-calculated, get the value from the cache
    if(!is.null(i)){
        message("getting cached data")
        return(i)
    }
    
    #Otherwise, get the value of the matrix and calculate its inverse
    data <- x$get()
    i <- solve(data,...)
    
    #set the value of the inverse
    x$setinverse(i)
    i
}