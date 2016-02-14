#function to create matrix that can be cached in global environment
makeCacheMatrix <- function(matrix_data = matrix()) {
  #assign to the cached matrix a new one
  set_matrix <- function(matrix_data) {
    cache_matrix <<- matrix_data
    #set the inverse value to null, so the program could know that there were
    #no inv computations
    inv_matrix <<- NULL
  }

  #return cached matrix
  get_matrix <- function() {
    cache_matrix
  }

  #assign cached inverse matrix to a new
  set_inverse_matrix <- function(matrix_data) {
    inv_matrix <<- matrix_data
  }

  #return cached inverse matrix
  get_inverse_matrix <- function() {
    inv_matrix
  }

  #auto call the constructor while using makeCacheMatrix() function
  set_matrix(matrix_data)

  #create and return list of available functions
  res <- list(
    se_matrixt = set_matrix,
    get_matrix = get_matrix,
    set_inverse_matrix = set_inverse_matrix,
    get_inverse_matrix = get_inverse_matrix
  )
  res
}

#function to find inverse matrix and save the solution into cache
cacheSolve <- function(matrix_data) {
  #get the inverse matrix
  cache_matrix <- matrix_data$get_inverse_matrix()
  #if it is already exists
  if(!is.null(cache_matrix)) {
    #print message
    message("getting cached data")
    #and return it
    cache_matrix
  #otherwise
  } else {
    #get the usual matrix
    cache_matrix <- matrix_data$get_matrix()
    #and find its iverse form by using solve() function
    inv_matrix <- solve(cache_matrix)
    #cahce the result
    matrix_data$set_inverse_matrix(inv_matrix)
    #and return it
    inv_matrix
  }
}
