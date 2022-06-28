sum_of_prime_factors <- function(x) {
  checkmate::assert_integerish(x)
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
