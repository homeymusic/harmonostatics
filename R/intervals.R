#########
#
# level 0
#

#########
#
# level 0
#

frequency.0.tonic.uncached <- function() {
  # for tonic-octave symmetry we use the equal temperament tritone
  numerator = c(1,16,9,6,5,4,sqrt(2),3,8,5,16,15,2)
  denominator = c(1,15,8,5,4,3,1,2,5,3,9,8,1)
  ratio = paste(numerator,":",denominator, sep="")

  # equal temperament tritone ratio label
  ratio[7] = "\u221A2:1"

  tibble::tibble(
    semitone = intervals.0()$semitone,
    name = intervals.0()$name,
    numerator = numerator,
    denominator = denominator,
    ratio = ratio
  )
}
frequency.0.tonic <- memoise::memoise(frequency.0.tonic.uncached)

frequency.0.octave.uncached <- function() {
  # from the octave's perspective the numerator and denominator of the tonic
  # frequency ratio are swapped and the order of the intervals is reversed
  t = frequency.0.tonic()
  numerator = rev(t$denominator)
  denominator = rev(t$numerator)
  ratio = paste(numerator,":",denominator, sep="")

  # equal temperament tritone ratio label
  ratio[7] = "1:\u221A2"

  tibble::tibble(
    semitone = intervals.0()$semitone,
    name = intervals.0()$name,
    numerator = numerator,
    denominator = denominator,
    ratio = ratio
  )
}
frequency.0.octave <- memoise::memoise(frequency.0.octave.uncached)

intervals.0.uncached <- function() {
  semtitones = 0:12
  names = c("tonic","minor 2nd","major 2nd","minor 3rd","major 3rd",
            "perfect 4th","tritone","perfect 5th","minor 6th",
            "major 6th","minor 7th","major 7th","octave")
  tibble::tibble(
    semitone = semtitones,
    name = names
  )
}
intervals.0 <- memoise::memoise(intervals.0.uncached)
