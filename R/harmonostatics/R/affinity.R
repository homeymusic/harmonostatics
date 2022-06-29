affinity.0 <- function() {
  tonic_disaffinity = disaffinity.0()$tonic
  octave_disaffinity = disaffinity.0()$octave

  tibble::tibble(
    name = intervals()$name,
    tonic = tonic_disaffinity %>% max - tonic_disaffinity,
    octave = octave_disaffinity %>% max - octave_disaffinity,
    mean = rbind(octave,tonic) %>% colMeans
  )
}

disaffinity.0 <- function() {
  t = tonic.frequency.0()
  tonic_disaffinity = calculate_disaffinity_with_et_tritone(t$numerator,t$denominator)

  o = octave.frequency.0()
  octave_disaffinity = calculate_disaffinity_with_et_tritone(o$numerator,o$denominator)

  tibble::tibble(
    name = intervals()$name,
    tonic = tonic_disaffinity,
    octave = octave_disaffinity,
    mean = rbind(octave,tonic) %>% colMeans
  )
}

calculate_disaffinity_with_et_tritone <- function(numerators, denominators) {
  # drop the irrational tritone
  numerators = numerators[-7]
  denominators = denominators[-7]
  # confirm the vectors are ready for prime operations
  checkmate::qassert(numerators,"X12")
  checkmate::qassert(denominators,"X12")

  # calculate disaffinity of remaining intervals
  disaffinity = calculate_disaffinity(numerators,denominators)

  # insert the ET tritone estimate into the results
  c(disaffinity[1:6],et_tritone_disaffinity(),disaffinity[7:12])
}

calculate_disaffinity <- function(numerators, denominators) {
  checkmate::assert_integerish(numerators)
  checkmate::assert_integerish(denominators)

  numerators %>% sapply(numbers::primeFactors) %>% sapply(sum_of_prime_factors) +
    denominators %>% sapply(numbers::primeFactors) %>% sapply(sum_of_prime_factors)
}

# Estimate Disaffinity of the ET Tritone
#
# We use the ET tritone which has a ratio of √2:1 with the tonic
# and 1:√2 with the octave
# Because √2 is not a positive integer we can't use prime factors to determine
# affinity. So for the tritone values we let symmetry and existing experimental
# data on consonance guide us.
#
# the lesser septimal tritone 7:5 is -17.5 cents from 600 and
# the greater septimal tritone 10:7 is +17.5 cents from 600
# we see this same reflection symmetry in all the other just intonation intervals
# for example:
# the Major 3rd is -13.7 cents from 400
# the minor 6th is +13.7 cents from 800
# the mean of the two septimal tritones ratios
# 7/5 = 1.400000
# 10/7 = 1.428571
# is 1.414286
# which is -7.215191e-05 from the sqrt(2)
# our assumption is that this small difference is beyond
# human detection in the context of musical harmony,
#
# For the affinity value, we take a small leap of intuition and use the mean of
# the affinities for the lesser septimal tritone and greater septimal tritones

et_tritone_disaffinity <- function() {
  lesser_septimal_tritone_disaffinity = sum(numbers::primeFactors(7),numbers::primeFactors(5))
  greater_septimal_tritone_disaffinity = sum(numbers::primeFactors(10),numbers::primeFactors(7))
  mean(c(lesser_septimal_tritone_disaffinity,greater_septimal_tritone_disaffinity))
}
