sum_of_prime_factors <- function(x) {
  checkmate::assert_integerish(x)
  # because 1 in exponential form would have all exponents = zero
  # we only sum prime numbers greater than 1
  # this is a shortcut versus creating the exponential form of all intervals
  sum(x[x>1])
}

rotate <- function(coordinates,angle) {
  checkmate::assert_numeric(angle)
  R = tibble::frame_matrix(
    ~x, ~y,
    cos(angle), -sin(angle),
    sin(angle), cos(angle)
  )
  R %*% coordinates
}

triangular_root <- function(x) {
  checkmate::assert_numeric(x)
  (sqrt(8*x+1)-1)/2
}
