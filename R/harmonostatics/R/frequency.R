tonic_frequency_ratios <-
  tibble::tibble(
    interval = 0:12,
    name = c("tonic","minor 2nd","major 2nd","minor 3rd","major 3rd",
            "perfect 4th","tritone","perfect 5th","minor 6th",
            "major 6th","minor 7th","major 7th","octave"),
    numerator = c(1,16,9,6,5,4,sqrt(2),3,8,5,16,15,2),
    denominator = c(1,15,8,5,4,3,1,2,5,3,9,8,1),
    ratio_label = paste(numerator,":",denominator, sep="")
)
tonic_frequency_ratios$ratio_label[7]<-"√2:1"

octave_frequency_ratios <-
  tibble::tibble(
    interval = tonic_frequency_ratios$interval,
    name = tonic_frequency_ratios$name,
    numerator = rev(tonic_frequency_ratios$denominator),
    denominator = rev(tonic_frequency_ratios$numerator),
    ratio_label = paste(numerator,":",denominator, sep="")
  )
octave_frequency_ratios$ratio_label[7]<-"1:√2"
