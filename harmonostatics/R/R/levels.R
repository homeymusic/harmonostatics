level_and_interval_for.uncached <- function(x,home) {
  checkmate::qassert(x,c("X==1","X==2"))
  checkmate::assert_choice(home,c(0,12))
  interval = 0
  if (x %>% length == 1) { x = c(home,x) }
  distance = abs(x[1] - x[2])
  if (distance <=12) {
    level = 0
    interval = distance
  } else {
    interval = distance %% 12
    level = (abs(distance - 12) / 12) %>% ceiling
    # intervals above the octave behave like the octave = 12
    if (interval == 0 && abs(max(x))-abs(min(x)) > 12) {
      interval = 12
    }
  }
  c(level=level,interval=interval)
}
level_and_interval_for <- memoise::memoise(level_and_interval_for.uncached)
