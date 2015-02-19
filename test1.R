library("testit")

source("cachematrix.R")

# sample x and its inverse for testing
xraw <- matrix(1:4, ncol=2)
expected <- matrix(c(-2,1,1.5,-0.5), ncol=2)

# test the constructor
x <- makeCacheMatrix(xraw)
assert("set/get", all.equal(xraw, x$get()))
assert("empty cache", is.null(x$getinverse()))

# test that the inverse is calculated correctly
xinv <- cacheSolve(x)
assert("inversion", all.equal(expected, xinv))

# test the cache
assert("setinverse/getinverse", all.equal(expected, x$getinverse()))

# test that the cached is being used
# should output "getting cached inverse\n" to stderr
xinv <- cacheSolve(x)
