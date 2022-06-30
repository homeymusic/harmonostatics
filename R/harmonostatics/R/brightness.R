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

  interval = level_and_interval_for(x)["interval"]
  brightness_polarity = harmony.0.brightness_polarity()[interval+1]
  brightness_for(brightness_polarity,orig_affinity)
}

brightness_for <- function(polarity,affinity) {
  # we use the stream function solution to the Laplace equation 2xy=const
  # with const = -2 and +2 for the relationship between brightness & affinity
  # x = 1 / y  ->  brightness = 1 / affinity
  centered_affinity = abs(affinity - harmony.0.brightness_boundary())
  ifelse(centered_affinity==0,
         0,
         polarity / centered_affinity
  )
}
