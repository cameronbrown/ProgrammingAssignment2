###########################################################################
##
## cachematrix.R
##
## makeCacheMatrix() and cacheSolve() provide a way to efficiently compute
## the inverse of a matrix.  Results are automatically cached so that repeated
## calls to cacheSolve() do not need to recompute.
##
## Example:
##   > m <- matrix(1:4, 2, 2)
##   > x <- makeCacheMatrix(m)
##   > x$get()                   # $get returns the matrix
##        [,1] [,2]
##   [1,]    1    3
##   [2,]    2    4
##   > inv <- cacheSolve(x)      # cacheSolve() will solve and cache
##   solving...
##   caching...
##   > inv
##        [,1] [,2]
##   [1,]   -2  1.5
##   [2,]    1 -0.5
##   > inv <- cacheSolve(x)      # subs. call gives previously cached solution
##   > inv
##        [,1] [,2]
##   [1,]   -2  1.5
##   [2,]    1 -0.5
##
###########################################################################


##
## makeCacheMatrix creates a specially wrapped square matrix object for
## use with cacheSolve().  The inverse is computed lazily and cached,
## minimizing the expense of repeated calls to cacheSolve().
##
## Usage
##   makeCacheMatrix(x)
##
## Arguments
##   x    a square numeric or complex matrix
##
## See Also
##   cacheSolve()
##

makeCacheMatrix <- function(x = matrix()) {

	if (!is.matrix(x) || ncol(x) != nrow(x)) {
		stop("argument must be a square matrix")
	}

	invcache <- NULL

	set <- function(x) {
		x <<- x
		invcache <<- NULL
	}

	get <- function() x

	setinv <- function(inv) invcache <<- inv

	getinv <- function() invcache

	list(set = set,
		get = get,
		setinv = setinv,
		getinv = getinv)
}


##
## cacheSolve() computes and returns the inverse of a matrix passed to
## makeCacheMatrix().  Subsequent calls to cacheSolve() for the same matrix
## will skip the computation and return a cached copy of the inverse.
##
## Usage
##   cacheSolve(x, ...)
##
## Arguments
##   x    an object returned by makeCacheMatrix()
##   ...  futher arguments are ignored
##
## See Also
##   makeCacheMatrix(), solve()
##

cacheSolve <- function(x, ...) {

	# check arguments
	if (!is.list(x) || is.null(x$getinv)) {
		stop("argument must have been created by makeCacheMatrix()")
	}
	if (length(list(...)) > 0) {
		warning(length(list(...)), " extra arguments ignored.")
	}

	## Return a matrix that is the inverse of 'x'
	xinv <- x$getinv()
	if (is.null(xinv)) {
		message("solving...")
		xinv <- solve(x$get())
		message("caching...")
		x$setinv(xinv)
	}
	return(xinv)
}




###########################################################################
## Unit Test for makeCacheMatrix() and cacheSolve()
###########################################################################

mcm.test <- function() {
	a <- matrix(c(-1, -2, 1, 1), 2,2)
	x <- makeCacheMatrix(a)
	inv <- cacheSolve(x)
	if (!identical(inv, solve(a))) stop()
	inv2 <- cacheSolve(x)
	if (!identical(inv, inv2)) stop()
}

suppressMessages(mcm.test())
rm(mcm.test)



###########################################################################
## The following is an implementation of the above that does not rely
## on lexical scoping.  This was just an experiment.
###########################################################################

makeCacheMatrix.NonLS <- function(x = matrix()) {
	if (!is.matrix(x) || ncol(x) != nrow(x)) {
		stop("argument must be a square matrix")
	}
	list(m = x, minv = NULL)
}

cacheSolve.NonLS <- function(x, ...) {
	# check arguments
	if (!is.list(x) || !is.matrix(x$m)) {
		stop("argument must have been created by makeCacheMatrix.NonLS()")
	}
	if (length(list(...)) > 0) {
		warning(length(list(...)), " extra arguments ignored.")
	}
	## Return a matrix that is the inverse of 'x'
	if (is.null(x$minv)) {
		message("solving & caching...")
		x$minv <- solve(x$m)
	}
	return(x$minv)
}

###########################################################################
## Unit Test for makeCacheMatrix.NonLS() and cacheSolve.NonLS()
###########################################################################

mcm.NonLS.test <- function() {
	a <- matrix(c(-1, -2, 1, 1), 2,2)
	x <- makeCacheMatrix.NonLS(a)
	inv <- cacheSolve.NonLS(x)
	if (!identical(inv, solve(a))) stop()
	inv2 <- cacheSolve.NonLS(x)
	if (!identical(inv, inv2)) stop()
}

suppressMessages(mcm.NonLS.test())
rm(mcm.NonLS.test)

