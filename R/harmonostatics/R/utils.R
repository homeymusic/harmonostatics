sum_of_prime_factors <- function(x) {
  checkmate::assert_integerish(x)
  # only prime numbers greater than 1 as 1 would have exponents of zero
  x[x>1] %>% sum
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
