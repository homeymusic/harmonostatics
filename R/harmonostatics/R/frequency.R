tonic.frequency <- function() {
  tibble::tibble(
    interval = 0:12,
    # for the tonic tritone we use 7:5
    # lesser septimal tritone (<tritone): -17.5 cents from 600
    name = c("tonic","minor 2nd","major 2nd","minor 3rd","major 3rd",
             "perfect 4th","<tritone","perfect 5th","minor 6th",
             "major 6th","minor 7th","major 7th","octave"),
    numerator = c(1,16,9,6,5,4,7,3,8,5,16,15,2),
    denominator = c(1,15,8,5,4,3,5,2,5,3,9,8,1),
    ratio = paste(numerator,":",denominator, sep="")
  )
}

octave.frequency <- function() {
  t = tonic.frequency()
  # from the octave's perspective the numerator and denominator of the tonic
  # are swapped and the order of the intervals is reversed
  n = rev(t$denominator)
  d = rev(t$numerator)
  # for octave tritone we use 7:10
  # greater septimal tritone (>tritone): +17.5 cents from 600
  n[7] = 7
  d[7] = 10
  t$name[7]= ">tritone"
  tibble::tibble(
    interval = t$interval,
    name = t$name,
    numerator = n,
    denominator = d,
    ratio = paste(numerator,":",denominator, sep="")
  )
}
