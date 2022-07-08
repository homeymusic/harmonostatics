position_and_level_from_integer.uncached <- function(x) {
  checkmate::qassert(x,"X1")
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
position_and_level_from_integer <- memoise::memoise(position_and_level_from_integer.uncached)

integer_from_position_and_level <- function(position,level) {
  checkmate::assert_choice(position,0:12)
  checkmate::qassert(level,"X1")
  12 * level + position
}
