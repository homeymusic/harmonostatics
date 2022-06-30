intervals.0 <- function() {
  semtitones = 0:12
  names = c("tonic","minor 2nd","major 2nd","minor 3rd","major 3rd",
            "perfect 4th","tritone","perfect 5th","minor 6th",
            "major 6th","minor 7th","major 7th","octave")
  tibble::tibble(
    semitone = semtitones,
    name = names
  )
}
