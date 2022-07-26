#########
#
# level 0
#

disaffinity_tonic.uncached <- function(norm="l1") {
  checkmate::assert_choice(norm,c("l1","l2"))
  t = frequency_ratio_tonic()
  calculate_disaffinity(t$numerator,t$denominator,norm)
}
disaffinity_tonic <- memoise::memoise(disaffinity_tonic.uncached)

disaffinity_octave.uncached <- function(norm="l1") {
  checkmate::assert_choice(norm,c("l1","l2"))
  o = frequency_ratio_octave()
  calculate_disaffinity(o$numerator,o$denominator,norm)
}
disaffinity_octave <- memoise::memoise(disaffinity_octave.uncached)

calculate_disaffinity <- function(numerators, denominators, norm="l1") {
  checkmate::assert_integerish(numerators)
  checkmate::assert_integerish(denominators)
  checkmate::assert_choice(norm,c("l1","l2"))

  if (norm=="l1") {
    l1norm(numerators,denominators)
  } else if (norm=="l2") {
    l2norm(numerators,denominators)
  }
}
