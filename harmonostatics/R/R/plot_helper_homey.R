colors_homey <- function() {
  list(
    'background' = '#664433',
    'foreground' = '#F3DDAB',
    'minor' = '#F3A904',
    'neutral' = '#FF5500',
    'major' = '#ABDAF3'
  )
}
colour_factor_homey <- function(x) {
  cut(x$brightness,c(-Inf,-1e-6,1e-6,Inf),labels=c("minor","neutral","major"))
}
color_values_homey <- function() {
  c("minor"=colors_homey()$minor,"neutral"=colors_homey()$neutral,"major"=colors_homey()$major)
}
theme_homey <- function(){
  font <- "Helvetica"   #assign font family up front

  ggplot2::theme_minimal()

  ggplot2::`%+replace%`  #replace elements we want to change

  ggplot2::theme(
    plot.title = ggplot2::element_text(color=colors_homey()$foreground),
    axis.title = ggplot2::element_text(color=colors_homey()$foreground),
    axis.text = ggplot2::element_text(color=colors_homey()$foreground),
    axis.ticks = ggplot2::element_blank(),
    plot.background = ggplot2::element_rect(fill = colors_homey()$background),
    panel.background = ggplot2::element_rect(fill = colors_homey()$background),
    panel.grid.major = ggplot2::element_line(color = colors_homey()$foreground, size=0.2),
    panel.grid.minor = ggplot2::element_line(color = colors_homey()$foreground, size=0.05, linetype ="dashed")
  )
}
