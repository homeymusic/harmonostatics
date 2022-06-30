#########
#
# level 0
#

frequency.0.tonic <- function() {
  # for tonic-octave symmetry we use the equal temperament tritone
  numerator = c(1,16,9,6,5,4,sqrt(2),3,8,5,16,15,2)
  denominator = c(1,15,8,5,4,3,1,2,5,3,9,8,1)
  ratio = paste(numerator,":",denominator, sep="")

  # equal temperament tritone ratio label
  ratio[7] = "√2:1"

  tibble::tibble(
    semitone = intervals.0()$semitone,
    name = intervals.0()$name,
    numerator = numerator,
    denominator = denominator,
    ratio = ratio
  )
}

frequency.0.octave <- function() {
  # from the octave's perspective the numerator and denominator of the tonic
  # frequency ratio are swapped and the order of the intervals is reversed
  t = frequency.0.tonic()
  numerator = rev(t$denominator)
  denominator = rev(t$numerator)
  ratio = paste(numerator,":",denominator, sep="")

  # equal temperament tritone ratio label
  ratio[7] = "1:√2"

  tibble::tibble(
    semitone = intervals.0()$semitone,
    name = intervals.0()$name,
    numerator = numerator,
    denominator = denominator,
    ratio = ratio
  )
}
