## BY Nicholas, <<EMAIL@gmail.com>>
## Can be used to take inverse of Matrices, but with added check to see if inverse
## already taken. Will help with large matrices so inverse doesn't need to be 
## calculated more than once.
## e.g.
## > mtx <- makeCacheMatrix(matrix(rnorm(9),3,3))
## > inverse_mtx <- cacheSolve(mtx)


## Function to take create "special matrix"
## Input: Numerical Matrix e.g. matrix(rnorm(9),3,3)
## Output: List of functions
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Function to take inverse of Matrix, but checks to see if 
## inverse already exists in Cache
## Input: "Special" Matrix output from `makeCacheMatrix`
## Output: Inverse of matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
              message("getting cached data")
              return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinv(m)
        m
}
