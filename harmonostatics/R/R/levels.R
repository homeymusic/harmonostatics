level_and_interval_for.uncached <- function(x,home=0) {
  checkmate::qassert(x,c("X==1","X==2"))
  checkmate::assert_choice(home,c(0,12))
  interval = 0
  if (x %>% length == 1) { x = c(home,x) }
  distance = abs(x[1] - x[2])
  if (distance <12) {
    level = 0
    interval = distance
  } else {
    interval = distance %% 12
    level = (abs(distance - 12) / 12) %>% ceiling
    # intervals above the octave behave like the octave = 12
    if (interval == 0 && abs(max(x))-abs(min(x)) > 0) {
      interval = 12
    }
  }
  c(level=level,interval=interval)
}
level_and_interval_for <- memoise::memoise(level_and_interval_for.uncached)

integer_from_position_and_level <- function(position,level) {
  checkmate::assert_choice(position,0:12)
  checkmate::qassert(level,"X1")
  12 * level + position
}
position_and_level_from_integer <- function(x) {
  checkmate::assert_integerish(x)
  position = NULL
  level = NULL
  if (x < 0) {
    position = x %% 12
    level = (x / 12) %>% floor
  } else if (x <= 12) {
    level = 0
    position = x
  } else if (x > 12) {
    position = x %% 12
    if (position == 0) {
      position = 12
    }
    level = ((x - 12) / 12) %>% ceiling
  }
  c(position,level)
}
