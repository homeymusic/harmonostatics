sum_of_prime_factors <- function(x) {
  checkmate::assert_integerish(x)
  # only prime numbers greater than 1 as 1 would have exponents of zero
  x[x>1] %>% sum
}

rotate <- function(coordinates,angle) {
  R = tibble::frame_matrix(
    ~x, ~y,
    cos(angle), -sin(angle),
    sin(angle), cos(angle)
  )
  R %*% coordinates
}
