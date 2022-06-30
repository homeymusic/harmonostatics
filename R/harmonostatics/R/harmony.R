harmony <- function(x, home, name=NULL) {
  checkmate::assert_integerish(x)
  checkmate::assert_choice(home,c(0,12))
  tibble(
    semitone = x %>% mean,
    intervallic_name = x %>% paste(collapse = ":"),
    name = name,
    affinity=affinity(x),
    brightness=brightness(x,home)
  )
}

affinity <- function(x) {
  checkmate::assert_integerish(x)
  ifelse (x %>% length < 2,
          calculate_affinity(x),
          combn(x,2,calculate_affinity) %>% mean
  )
}

calculate_affinity <- function(x) {
  checkmate::qassert(x,c("X==1","X==2"))
  level_and_interval = level_and_interval_of(x)
  level = level_and_interval["level"]
  interval = level_and_interval["interval"]
  level_penalty = 2 * abs(level)
  if (abs(level) == 1) {level_penalty = 1}
  harmony.0.affinity()[interval+1] - level_penalty
}

brightness <- function(x,home) {
  checkmate::assert_choice(home,c(0,12))
  checkmate::assert_integerish(x)
  sapply(x,calculate_brilliance,home) %>% mean
}

calculate_brilliance <- function(x,home) {
  checkmate::qassert(x,"X1")
  checkmate::assert_choice(home,c(0,12))

  orig_affinity = affinity(x)

  ifelse(home==0,
         (x = x - min(x)),
         (x = x + 12 - max(x))
  )

  interval = level_and_interval_of(x)["interval"]
  brightness_polarity = harmony.0.brightness_polarity()[interval+1]
  brightness_for(brightness_polarity,orig_affinity)
}

# below are the fundamentals for level 0
# level 0 is one octave between the tonic and the octave
# we use the word level instead of octave because throughout
# because of the name space collision between tonic and octave

harmony.0.rotation_angle <- function() {
  # use the tritone to determine the rotation angle
  tritone_i = 6 + 1
  atan2(affinity.0.octave()[tritone_i],affinity.0.tonic()[tritone_i])
}
harmony.0.affinity_brightness_polarity <-function() {
  (rbind(affinity.0.tonic(),affinity.0.octave()) %>%
     rotate(harmony.0.rotation_angle()) * cos(harmony.0.rotation_angle())) %>% zapsmall
}
harmony.0.affinity <- function() {
  # Directional Derivative:
  # rotate around the origin by the rotation angle
  # changing the coordinate system
  # from: octave-affinity versus tonic-affinity
  # to: octave-tonic-affinity versus brightness-polarity
  harmony.0.affinity_brightness_polarity()[2,]
}
harmony.0.brightness_polarity <- function() {
  # Directional Derivative:
  # rotate around the origin by the rotation angle
  # changing the coordinate system
  # from: octave-affinity versus tonic-affinity
  # to: octave-tonic-affinity versus brightness-polarity
  harmony.0.affinity_brightness_polarity()[1,]
}

harmony.0.brightness_boundary <- function() {
  # we need more experimental data to determine the origin boundary for
  # brightness. our current approach uses the triangular nature of affinity
  # (1,3,6,10,15) to make a best guess that also aligns with the
  # experimental data with the Major 3rd and minor 6th
  # having the greatest positive and negative values of brightness.
  harmony.0.affinity() %>% max %>% triangular_root
}

harmony.0.brightness <- function() {
  brightness_for(harmony.0.brightness_polarity(), harmony.0.affinity())
}

# TODO: replace this with a general function that takes polarity and affinity
# and returns brightness so that we don't repeat it later
# brightness_for(polarity,affinity)
brightness_for <- function(polarity,affinity) {
  # we use the stream function solution to the Laplace equation 2xy=const
  # with const = -2 and +2 for the relationship between brightness & affinity
  centered_affinity = abs(affinity - harmony.0.brightness_boundary())
  ifelse(centered_affinity==0,
         0,
         polarity / centered_affinity
  )
}

affinity.0.tonic <- function() {
  tonic_disaffinity = disaffinity.0.tonic()
  tonic_disaffinity %>% max - tonic_disaffinity
}
affinity.0.octave <- function() {
  octave_disaffinity = disaffinity.0.octave()
  octave_disaffinity %>% max - octave_disaffinity
}
disaffinity.0.tonic <- function() {
  t = tonic.frequency.0()
  tonic_disaffinity = calculate_disaffinity_with_equal_temperament_tritone(t$numerator,t$denominator)
}
disaffinity.0.octave <- function() {
  o = octave.frequency.0()
  octave_disaffinity = calculate_disaffinity_with_equal_temperament_tritone(o$numerator,o$denominator)
}

calculate_disaffinity_with_equal_temperament_tritone <- function(numerators, denominators) {
  # drop the irrational tritone
  numerators = numerators[-7]
  denominators = denominators[-7]
  # confirm the vectors are ready for prime operations
  checkmate::qassert(numerators,"X12")
  checkmate::qassert(denominators,"X12")

  # calculate disaffinity of remaining intervals
  disaffinity = calculate_disaffinity(numerators,denominators)

  # insert the ET tritone estimate into the results
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
