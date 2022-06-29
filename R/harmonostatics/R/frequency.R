# for tonic-octave symmetry and applications to equal temperament tuning
# we use the equal temperament tritone
tonic.frequency.0 <- function() {

  numerator = c(1,16,9,6,5,4,sqrt(2),3,8,5,16,15,2)
  denominator = c(1,15,8,5,4,3,1,2,5,3,9,8,1)
  ratio = paste(numerator,":",denominator, sep="")

  # tritone ratio label
  ratio[7] = "√2:1"

  tibble::tibble(
    name = intervals.0()$name,
    numerator = numerator,
    denominator = denominator,
    ratio = ratio
  )
}

octave.frequency.0 <- function() {

  # from the octave's perspective the numerator and denominator of the tonic
  # are swapped and the order of the intervals is reversed
  t = tonic.frequency.0()
  numerator = rev(t$denominator)
  denominator = rev(t$numerator)
  ratio = paste(numerator,":",denominator, sep="")

  # tritone ratio label
  ratio[7] = "1:√2"

  tibble::tibble(
    name = t$name,
    numerator = numerator,
    denominator = denominator,
    ratio = ratio
  )
}
