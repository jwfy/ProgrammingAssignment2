## creates a special matrix and put the inverse value act as a cache variable
makeCacheMatrix <- function(x = matrix()) {
    selfInvMat <- NULL
    # init the variable
	
    set <- function(y){
        x <<- y
        selfInvMat <<- NULL
    }
    # set function and set x value as the y
    # and define the class of x as the matrix
	
    get <- function() x
    # return the x
	
    setInv <- function(solve) selfInvMat <<- solve
    # calculate the inverse of x and add to the cache
	
    getInv <- function() selfInvMat
    # get the inverse value of x
	
    list(set=set, get=get, setInv=setInv, getInv=getInv)
    # return all the function 
}


## first get the inverse of the x,second analyse the value.
## if the value has bean calculated we should return the value,
## otherwise througu the "solve" function calculate the inverse of the x
## and set the cache variable
## finally return the value

cacheSolve <- function(x, ...) {
    selfInvMax <- x$getInv()
	# get the value

    if(!is.null(selfInvMax)){
        message("getting cached data")
        return(selfInvMax)
    }
	# cache has bean calculated ,return the value
	
    data <- x$get()
    # set the matrix value
	
    selfInvMax <- solve(data, ...)
    # calculated the inverse
	
    x$setInv(selfInvMax)
    # save the value
    
    selfInvMax
    # return the inverse
}
