# Programming_Assignment2

#Function “makeCacheMatrix” creates a special “matrix” object that can cache its inverse. makeCacheMatrix #contains 4 functions: set, get, setinverse, getinverse.
makeCacheMatrix <- function(x = matrix()) {
+     m <- NULL
+     set <- function(y) {
+         x <<- y
+         m <<- NULL
+     }
+     get <- function() x
+     setinverse <- function(solve) m <<- solve
+     getinverse <- function() m
+     list(set = set, get = get,
+          setinverse = setinverse,
+          getinverse = getinverse)
+ }
#Function “cacheSolve” computes the inverse of the special “matrix” returned by makeCacheMatrix. 
If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve 
the inverse from the cache. If the inverse has not been calculated, data gets the matrix stored with makeCacheMatrix, 
m calculates the inverse, and x$inverse(m) stores it in the object m in makeCacheMatrix.#
cacheSolve <- function(x, ...) {
+     m <- x$getinverse()
+     if(!is.null(m)) {
+         message("getting cached data")
+         return(m)
+     }
+     data <- x$get()
+     m <- solve(data, ...)
+     x$setinverse(m)
+     m
+ }
># Applying data
> suro<-diag(6,3)
> suro
     [,1] [,2] [,3]
[1,]    6    0    0
[2,]    0    6    0
[3,]    0    0    6
> Cache_Marix <- makeCacheMatrix(suro)
> cacheSolve(Cache_Marix)
          [,1]      [,2]      [,3]
[1,] 0.1666667 0.0000000 0.0000000
[2,] 0.0000000 0.1666667 0.0000000
[3,] 0.0000000 0.0000000 0.1666667
># On applying the above functions , getting the cached data. Thanks for reviewing.
