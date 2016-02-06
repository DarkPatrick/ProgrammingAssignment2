makeCacheMatrix <- function(matrix_data = matrix()) {
  set_matrix <- function(matrix_data) {
    cache_matrix <<- matrix_data
    inv_matrix <<- NULL
  }

  get_matrix <- function() {
    cache_matrix
  }

  set_inverse_matrix <- function(matrix_data) {
    inv_matrix <<- matrix_data
  }

  get_inverse_matrix <- function() {
    inv_matrix
  }

  set_matrix(matrix_data)

  res <- list(
    se_matrixt = set_matrix,
    get_matrix = get_matrix,
    set_inverse_matrix = set_inverse_matrix,
    get_inverse_matrix = get_inverse_matrix
  )
  res
}

cacheSolve <- function(matrix_data) {
  cache_matrix <- matrix_data$get_inverse_matrix()
  if(!is.null(cache_matrix)) {
    message("getting cached data")
    cache_matrix
  } else {
    cache_matrix <- matrix_data$get_matrix()
    inv_matrix <- solve(cache_matrix)
    matrix_data$set_inverse_matrix(inv_matrix)
    inv_matrix
  }
}
