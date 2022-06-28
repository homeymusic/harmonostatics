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

affinity <- function() {
  i = intervals()
  t_d = tonic.disaffinity()
  o_d = octave.disaffinity()

  tibble::tibble(
    name = i$name,
    tonic = t_d %>% max - t_d,
    octave = o_d %>% max - o_d,
    mean = rbind(octave,tonic) %>% colMeans
  )
}
