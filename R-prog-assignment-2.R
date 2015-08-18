makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(matrix) m <<- matrix
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


cacheSolve<- function(x, ...) {
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  mat.data <- x$get()
  m <- solve(mat.data)
  x$setmatrix(m)
  m
}




#> x = matrix(c(1,5,32,0,0,2,5,9,1),nrow =3)
#> m = makeCacheMatrix(x)
#> m$get()
#[,1] [,2] [,3]
#[1,]    1    0    5
#[2,]    5    0    9
#[3,]   32    2    1
#> cacheSolve(m)
#[,1]     [,2] [,3]
#[1,] -0.56250  0.31250  0.0
#[2,]  8.84375 -4.96875  0.5
#[3,]  0.31250 -0.06250  0.0