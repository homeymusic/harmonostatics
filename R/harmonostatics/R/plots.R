theme_homey <- function(){
  brown = '#664433'
  cream = '#F3DDAB'

  font <- "Helvetica"   #assign font family up front

  ggplot2::theme_minimal()

  ggplot2::`%+replace%`  #replace elements we want to change

  ggplot2::theme(
    # text = ggplot2::element_text(size = 5),
    plot.title = ggplot2::element_text(color=cream),
    axis.title = ggplot2::element_text(color=cream),
    axis.text = ggplot2::element_text(color=cream),
    axis.ticks = ggplot2::element_blank(),
    plot.background = ggplot2::element_rect(fill = brown),
    panel.background = ggplot2::element_rect(fill = brown),
    panel.grid.major = ggplot2::element_line(color = cream, size=0.2),
    panel.grid.minor = ggplot2::element_line(color = cream, size=0.05, linetype ="dashed")
  )
}
