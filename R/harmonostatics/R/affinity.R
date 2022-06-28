disaffinity <- function() {
  t = tonic.frequency()
  t_d = t$numerator %>% sapply(numbers::primeFactors) %>% sapply(sum_of_prime_factors) +
    t$denominator %>% sapply(numbers::primeFactors) %>% sapply(sum_of_prime_factors)

  o = octave.frequency()
  o_d = o$numerator %>% sapply(numbers::primeFactors) %>% sapply(sum_of_prime_factors) +
    o$denominator %>% sapply(numbers::primeFactors) %>% sapply(sum_of_prime_factors)

  tibble::tibble(
    name = intervals()$name,
    tonic = t_d,
    octave = o_d,
    mean = rbind(octave,tonic) %>% colMeans
  )
}


affinity <- function() {
  t_d = disaffinity()$tonic
  o_d = disaffinity()$octave

  tibble::tibble(
    name = intervals()$name,
    tonic = t_d %>% max - t_d,
    octave = o_d %>% max - o_d,
    mean = rbind(octave,tonic) %>% colMeans
  )
}
