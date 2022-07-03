#' Plot Harmony
#'
#' Provides scatter plots of musical harmony metrics of a note or chord.
#'
#' @param x A list of notes or chords expressed as an interval integers or vector of interval integers
#' @param home The home pitch expressed an as interval integer
#' @param columns A vector of 2 column names to plot: semitone, affinity, brightness and magnitude
#' @param unlist=FALSE A logical TRUE or FALSE to plot points individually or as one point
#' @param include_names=TRUE Include the names from the list x on the plot
#' @param title=NULL An optional title for the plot
#' @return Generates the requested scatter plot and returns TRUE
#'
#' @examples
#' plot_harmony(combn(0:12,2,simplify=FALSE),home=0,columns=c("brightness","affinity"),title="all 2 note chords in level 0 tonic home")
#' plot_harmony(combn(0:12,2,simplify=FALSE),home=12,columns=c("brightness","affinity"),title="all 2 note chords in level 0 octave home")
#'
#' @export
plot_harmony <- function(x,home,columns,unlist=FALSE,include_names=TRUE,title=NULL) {
  if (is.null(names(x))) {include_names=FALSE}
  checkmate::assert(checkmate::check_list(x,types="integerish"))
  checkmate::assert_choice(home,c(0,12))
  checkmate::qassert(columns,"S2")
  checkmate::assert_logical(unlist)
  checkmate::assert_logical(include_names)
  checkmate::assert_character(title,null.ok=TRUE)

  h = x
  if (unlist) { x = x %>% unlist}
  if (include_names) {
    n = names(x)
    l = list(x=x,name=n)
    h = purrr::pmap(l,harmony,home=home) %>% purrr::map_dfr(.f=dplyr::bind_rows)
  } else {
    h = purrr::map(x,harmony,home=home) %>% purrr::map_dfr(.f=dplyr::bind_rows)
  }
  plot(h[,columns],main=title)
  text(h[,columns],labels=h$intervallic_name,pos=1)
  if (include_names) {
    text(h[,columns],labels=h$name,pos=3)
  }
  TRUE
}

#' Homey Plot Harmony
#'
#' Provides scatter plots of musical harmony metrics of a note or chord with the homey theme.
#'
#' @param x A list of notes or chords expressed as an interval integers or vector of interval integers
#' @param home The home pitch expressed an as interval integer
#' @param columns A vector of 2 column names to plot: semitone, affinity, brightness and magnitude
#' @param unlist=FALSE A logical TRUE or FALSE to plot points individually or as one point
#' @param include_names=TRUE Include the names from the list x on the plot
#' @param title=NULL An optional title for the plot
#' @return Generates the requested scatter plot and returns TRUE
#'
#' @examples
#' homey_plot_harmony(combn(0:12,2,simplify=FALSE),home=0,columns=c("brightness","affinity"),title="all 2 note chords in level 0 tonic home")
#' homey_plot_harmony(combn(0:12,2,simplify=FALSE),home=12,columns=c("brightness","affinity"),title="all 2 note chords in level 0 octave home")
#'
#' @export
homey_plot_harmony <- function(x,home,columns,unlist=FALSE,include_names=TRUE,title=NULL) {
  if (is.null(names(x))) {include_names=FALSE}
  checkmate::assert(checkmate::check_list(x,types="integerish"))
  checkmate::assert_choice(home,c(0,12))
  checkmate::qassert(columns,"S2")
  checkmate::assert_logical(unlist)
  checkmate::assert_logical(include_names)
  checkmate::assert_character(title,null.ok=TRUE)

  h = x
  if (unlist) { x = x %>% unlist}
  if (include_names) {
    n = names(x)
    l = list(x=x,name=n)
    h = purrr::pmap(l,harmony,home=home) %>% purrr::map_dfr(.f=dplyr::bind_rows)
  } else {
    h = purrr::map(x,harmony,home=home) %>% purrr::map_dfr(.f=dplyr::bind_rows)
  }

  colour_factor = colour_factor_homey(h)
  color_values = color_values_homey()
  h %>% ggplot2::ggplot(ggplot2::aes_string(x = columns[1], y = columns[2], colour=colour_factor)) +
    ggplot2::geom_point() +
    ggplot2::scale_color_manual(values = color_values, guide="none") +
    ggplot2::geom_label(ggplot2::aes(label=name),label.size = NA,fill=NA,vjust='bottom',hjust="outward",label.padding = ggplot2::unit(0.3, "lines")) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0.6)) +
    ggplot2::ggtitle(title) +
    theme_homey()
}
