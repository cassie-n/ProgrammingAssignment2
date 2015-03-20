## Here are two functions: makeCacheMatrix and cacheSolve.

##The first, makeCacheMatrix, is a function which reformats a matrix into a list of functions, but the result is effectively a matrix which is able to cache its inverse.

##The second, cacheSolve, is a function whose input is a matrix in the format provded by makeCacheMatrix, and the output is the inverse of the matrix.  If the inverse has been cached, cacheSolve will output the cached value.  Otherwise it will calculate the inverse, cache it into the matrix, and output the inverse.




## makeCacheMatrix inputs a matrix and outputs a list of functions.

 ## set is the first function in the list of functions.  It just sets x to be the matrix which was our original input.
    ## get is the second function in the list of functions.  It outputs x (the matrix).
    ## setinv is the third function in the list of functions.  It creates a way of changing the inverse later on (that's why cacheSolve uses it).
    ## getinv is the function which outputs the cached inverse (established in setinv).  Note that without running the function cacheSolve, the cached inverse is null.
    ## The output of makeCacheMatrix is a list of the four above functions.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
        set <- function(y) {
        x <<- y
        m <<- NULL
    }
        get <- function() {
        x
    }
        setinv <- function(inv) {
        m <<- inv
    }
        getinv <- function() {
        m
    }

    list(set=set, get=get, setinv = setinv, getinv = getinv)

}


## cacheSolve inputs a matrix (in makeCacheMatrix format) and outputs the cached inverse if it exists.  Otherwise it computes the inverse, caches the inverse, and outputs the inverse.
    ## set m equal to the cached value
         ## if it is not null, it finds it in the cache, and returns the inverse
        ## if the cached inverse is null, we set data equal to the matrix (in standard matrix format).  Then we compute the inverse, assign that value to m, cache the value m with setinv, and output m.

cacheSolve <- function(x) {

         m <- x$getinv()
         if(!is.null(m)) {
                 message("getting cached inverse")
                 return(m)
             }
         else {
                 data <- x$get()
                 m <- solve(data)
                 x$setinv(m)
                 return(m)
             }
     }



