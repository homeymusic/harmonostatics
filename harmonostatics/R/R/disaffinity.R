#########
#
# level 0
#

disaffinity.0.tonic <- function() {
  t = frequency.0.tonic()
  tonic_disaffinity = calculate_disaffinity_with_equal_temperament_tritone(t$numerator,t$denominator)
}

disaffinity.0.octave <- function() {
  o = frequency.0.octave()
  octave_disaffinity = calculate_disaffinity_with_equal_temperament_tritone(o$numerator,o$denominator)
}

calculate_disaffinity_with_equal_temperament_tritone <- function(numerators, denominators) {
  # drop the irrational tritone in order to calculate the other prime intervals
  numerators = numerators[-7]
  denominators = denominators[-7]
  # confirm the vectors are ready for prime operations
  checkmate::qassert(numerators,"X12")
  checkmate::qassert(denominators,"X12")

  # calculate disaffinity of remaining intervals
  disaffinity = calculate_disaffinity(numerators,denominators)

  # insert the equal temperament tritone estimate back into the results
  c(disaffinity[1:6],equal_temperament_tritone_disaffinity(),disaffinity[7:12])
}

calculate_disaffinity <- function(numerators, denominators) {
  checkmate::assert_integerish(numerators)
  checkmate::assert_integerish(denominators)

  numerators %>% sapply(numbers::primeFactors) %>% sapply(sum_of_prime_factors) +
    denominators %>% sapply(numbers::primeFactors) %>% sapply(sum_of_prime_factors)
}

# Estimate Disaffinity of the Equal Temperament Tritone
#
# We use the equal temperament tritone which has a ratio of √2:1 with the tonic
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

equal_temperament_tritone_disaffinity <- function() {
  lesser_septimal_tritone_disaffinity = sum(numbers::primeFactors(7),numbers::primeFactors(5))
  greater_septimal_tritone_disaffinity = sum(numbers::primeFactors(10),numbers::primeFactors(7))
  mean(c(lesser_septimal_tritone_disaffinity,greater_septimal_tritone_disaffinity))
}
