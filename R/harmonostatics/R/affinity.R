sum_of_prime_factors <- function(x) {
  checkmate::assert_integerish(x)
  # only prime numbers greater than 1 as 1 would have exponents of zero
  x[x>1] %>% sum
}

tonic.disaffinity <- function() {
  t = tonic.frequency()
  t$numerator %>% sapply(numbers::primeFactors) %>% sapply(sum_of_prime_factors) +
    t$denominator %>% sapply(numbers::primeFactors) %>% sapply(sum_of_prime_factors)
}

octave.disaffinity <- function() {
  o = octave.frequency()
  o$numerator %>% sapply(numbers::primeFactors) %>% sapply(sum_of_prime_factors) +
    o$denominator %>% sapply(numbers::primeFactors) %>% sapply(sum_of_prime_factors)
}

tonic.affinity <- function() {
  t_d = tonic.disaffinity()
  t_d %>% max - t_d
}

octave.affinity <- function() {
  o_d = octave.disaffinity()
  o_d %>% max - o_d
}

