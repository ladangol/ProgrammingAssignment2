## makeCacheMatrix is similar to makeVector in the example , returning a list of four functions that can set or get the matrix x and get and set its inverse
## 

## A special matrix, it's special because we can get its cached inverse.
## It will return a list of four functions , their functionality is straightforward
makeCacheMatrix <- function(x = matrix()) {
        inverse<-NULL
	set <- function(y){
	    x<<-y
            inverse<<-NULL
        }
	get <-function() x
        setinverse <- function(inv) inverse<<-inv
	getinverse <- function() inverse
 	list(set = set,get =get , 
	      setinverse = setinverse,
	      getinverse = getinverse) 
}


##cacheSolve calculates the inverse of a special matrix if it has not already been calculated
## and then use the cache inverse unless there is a change in the matrix so
##it has to be calculated again 

cacheSolve <- function(x, ...) {
        minv <- x$getinverse()
	if(!is.null(minv)){
	     message("getting cached inverse")
	     return(minv)
	}
        mat <- x$get()
        minv <- solve(mat,...)
        x$setinverse(minv)

        minv
}
