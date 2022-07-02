plot_harmony <- function(x,home,columns,home_chord=NULL,unlist=FALSE,include_names=TRUE,name=NULL,title=NULL) {
  if (is.null(names(x))) {include_names=FALSE}
  checkmate::assert(checkmate::check_list(x,types="integerish"))
  checkmate::assert_choice(home,c(0,12))
  checkmate::qassert(columns,"S2")
  h = x
  if (unlist) { x = x %>% unlist}
  if (include_names) {
    n = names(x)
    l = list(x=x,name=n)
    h = purrr::pmap(l,harmony,home=home,home_chord=home_chord) %>% purrr::map_dfr(.f=dplyr::bind_rows)
  } else {
    h = purrr::map(x,harmony,home=home,home_chord=home_chord) %>% purrr::map_dfr(.f=dplyr::bind_rows)
  }
  plot(h[,columns],main=title)
  text(h[,columns],labels=h$intervallic_name,pos=1)
  if (include_names) {
    text(h[,columns],labels=h$name,pos=3)
  }
  TRUE
}

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
