level_and_interval_of <- function(x) {
  checkmate::qassert(x,c("X==1","X==2"))
  if (x %>% length == 1) {x = c(0,x)}
  s = abs(max(x))-abs(min(x))
  interval = s%%12
  if (s<0) {
    level = trunc(s/12) - 1
    if (interval == 0) {
      level=level+1
    }
  } else if (s<12) {
    level = 0
  } else if (s>=12) {
    level = trunc(s/12)
    if (interval == 0) {
      level=level-1
      interval = 12
    }
  }
  c(level=level,interval=interval)
}
