###

# object x is initialized as a function argument
makeVector <- function(x = numeric()) {
# object m is initialized as an object, within the makeVector() 
# environment to be used by later code in the function
     m <- NULL
# First, the set function is defined
     set <- function(y) {
# The <<- assignment operator, assigns the value on the right side
# of the operator (y and NULL) to an object in the parent environment
# named (x and m)
# if there is already a mean cached in m, whenever x is reset, 
# the value of m cached in the memory is cleared. Therefore, it forces
# a new calculation of the mean for the current x values.
          x <<- y
          m <<- NULL
     }
# The get function is defined
# The x is not defined in the get function. So, R looks for it in the
# parent environment, using the lexical scoping.
     get <- function() x
# the mean m is setted; the <<- operator assigns the mean value 
# defined in the parent directory to m
     setmean <- function(mean) m <<- mean
# The mean m is then defined for the getter
     getmean <- function() m
# The functions defined above [set(), get(), setmean() and getmean()] 
# are assigned as an element within a list, returning the elements 
# to the parent environment naming them [set, get, setmean and getmean]
     list(set = set, get = get,
          setmean = setmean,
          getmean = getmean)
# Naming the functions this way allows us to retrieve their contents
# using the $ form of the extract operator
}
cachemean <- function(x, ...) {
     m <- x$getmean()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- mean(data, ...)
     x$setmean(m)
     m
}