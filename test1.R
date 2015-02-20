library("testit")

source("cachematrix.R")

# sample x and its inverse for testing
y1 <- matrix(1:4, ncol=2)
y1inv <- matrix(c(-2,1,1.5,-0.5), ncol=2)
y2 <- matrix(c(7:10), ncol=2)
y2inv <- matrix(c(-5,4,4.5,-3.5), ncol=2)

# base cases
x <- makeCacheMatrix()
assert("x is na", all.equal(is.na(x$get()), is.na(matrix())))
assert("empty cache", is.null(x$getinverse()))
xinv <- cacheSolve(x)
assert("inverse is na", all.equal(is.na(xinv), is.na(matrix())))

# test the constructor
x <- makeCacheMatrix(y1)
assert("set/get", all.equal(y1, x$get()))
assert("empty cache", is.null(x$getinverse()))

# test that the inverse is calculated correctly
xinv <- cacheSolve(x)
assert("inversion", all.equal(y1inv, xinv))

# test the cache
assert("setinverse/getinverse", all.equal(y1inv, x$getinverse()))

# test that the cached is being used
# should output "getting cached inverse\n" to stderr
xinv <- cacheSolve(x)
assert("correct cache", all.equal(y1inv, xinv))

# test that the inverse is reset if x is changed
x$set(y2)
assert("x reset", all.equal(y2, x$get()))
assert("xinv reset", is.null(x$getinverse()))

# test that the inverse is re-calculated correctly
xinv <- cacheSolve(x)
assert("inversion", all.equal(y2inv, xinv))

# test the cache
assert("setinverse/getinverse", all.equal(y2inv, x$getinverse()))

# test that the cached is being used
# should output "getting cached inverse\n" to stderr
xinv <- cacheSolve(x)
assert("correct cache", all.equal(y2inv, xinv))

