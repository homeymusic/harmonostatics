test_that("we can create a euler style polygon of triads", {
  t = tibble::tibble(
    angle = c(0,1/2,1,1.5,2),
    length=rep(1,5),
    group=rep(1,5)
  )
  ggplot2::ggplot(data = t, ggplot2::aes(x = angle, y = length, group = 1)) +
    ggplot2::ylim(0, NA) +
    ggplot2::geom_point(color = 'purple', stat = 'identity') +
    ggplot2::geom_polygon(color = 'purple', fill=NA) +
    ggplot2::coord_polar()
})
