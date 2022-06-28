rotate <- function(coordinates,angle) {
  R = tibble::frame_matrix(
    ~x, ~y,
    cos(angle), -sin(angle),
    sin(angle), cos(angle)
  )
  R %*% coordinates
}
