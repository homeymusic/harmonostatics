frequency.tonic <- function() {
  t = tibble::tibble(
    interval = 0:12,
    name = c("tonic","minor 2nd","major 2nd","minor 3rd","major 3rd",
             "perfect 4th","tritone","perfect 5th","minor 6th",
             "major 6th","minor 7th","major 7th","octave"),
    numerator = c(1,16,9,6,5,4,sqrt(2),3,8,5,16,15,2),
    denominator = c(1,15,8,5,4,3,1,2,5,3,9,8,1),
    ratio = paste(numerator,":",denominator, sep="")
  )
  t$ratio[7]<-"√2:1"
  t
}

frequency.octave <- function() {
  tonic = frequency.tonic()
  t = tibble::tibble(
    interval = tonic$interval,
    name = tonic$name,
    numerator = rev(tonic$denominator),
    denominator = rev(tonic$numerator),
    ratio = paste(numerator,":",denominator, sep="")
  )
  t$ratio[7]<-"1:√2"
  t
}
