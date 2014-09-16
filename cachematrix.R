# The two functions below perform matrix inversion of a sqaure matrix(same number of rows and columns)
# We have assumed that input is always a sqaured matrix
# Inverse of a matrix is calculated using solve function of base R(?solve)

######working example
# mat = replicate(10,rnorm(10))
# mat.list = makeCacheMatrix(mat)
# cacheSolve(mat.list)
###### 

## Following function consumes a matrix argument and returns a list of four functions
## set function - it takes a value y and sets it to x within the makeCacheMatrix env. 
## when called from makeCacheMatrix function, it will set x to y and m to NULL in makeCacheMatrix env.
## get function - returns initial argument x
## setinverse function - assigns initial argument inv to m to its immediate parent function makeCacheMatrix
## getinverse funtion - is similar to get function and returns initial argument m 
makeCacheMatrix <- function(x = matrix()) {
        
        #initialize m to NULL
        m <- NULL
        
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) m <<- inv
        getinverse <- function() m
        #return list of functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)     
}

## Write a short comment describing this function

## Following function consumes x which is the "special" list of function returned by makeCachematrix
## it first calls getinverse and checks if the inverse has been calculated in makeCachematrix
## if yes then it returns inverse cached in variable m
## if no, then it fetches matrix from get()in makeCachematrix
## remember now that get <- function() z is as good as get <- z so, get() simply gets
## us the square matrix we passed with our special list x
## inverse is calculated and stored in m
## setinverse function of makeCachematrix is called and m is set to inverse in 
## both cacheSolve and makeCachematrix env.
## m is printed
cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
        
}
