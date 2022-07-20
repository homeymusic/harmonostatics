position_and_level_from_integer.uncached <- function(x) {
  checkmate::qassert(x,"X1")
  position = NULL
  level = NULL
  if (x < 0) {
    position = x %% 12
    level = (x / 12) %>% floor
  } else if (x <= 12) {
    level = 0
    position = x
  } else if (x > 12) {
    position = x %% 12
    if (position == 0) {
      position = 12
    }
    level = ((x - 12) / 12) %>% ceiling
  }
  c(position,level)
}
position_and_level_from_integer <- memoise::memoise(position_and_level_from_integer.uncached)

integer_from_position_and_level <- function(position,level) {
  checkmate::assert_choice(position,0:12)
  checkmate::qassert(level,"X1")
  12 * level + position
}

sum_of_prime_factors <- function(x) {
  checkmate::assert_integerish(x)
  # because 1 in exponential form would have all exponents = zero
  # we only sum prime numbers greater than 1
  # this is a shortcut versus creating the exponential form of all intervals
  sum(x[x>1])
}

sum_of_sq_prime_factors <- function(x) {
  checkmate::assert_integerish(x)
  # because 1 in exponential form would have all exponents = zero
  # we only sum prime numbers greater than 1
  # this is a shortcut versus creating the exponential form of all intervals
  sum(x[x>1]^2)
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
l1norm <- function(numerators, denominators) {
  numerators %>% sapply(numbers::primeFactors) %>% sapply(sum_of_prime_factors) +
  denominators %>% sapply(numbers::primeFactors) %>% sapply(sum_of_prime_factors)
}
l2norm <- function(numerators, denominators) {
  (numerators %>% sapply(numbers::primeFactors) %>% sapply(sum_of_sq_prime_factors) +
    denominators %>% sapply(numbers::primeFactors) %>% sapply(sum_of_sq_prime_factors)) %>% sqrt
}
