#' Plot Potential Energy
#'
#' Provides scatter plots of potential energy between two notes or chords
#'
#' @param x A list of notes or chords expressed as an interval integers or vector of interval integers
#' @param y A list of notes or chords expressed as an interval integers or vector of interval integers
#' @param home The home pitch expressed an as interval integer
#' @param columns A vector of 2 column names to plot: semitone, affinity, brightness and magnitude
#' @param unlist=FALSE A logical TRUE or FALSE to plot points individually or as one point
#' @param include_names=TRUE Include the names from the list x on the plot
#' @param title=NULL An optional title for the plot
#' @return Generates the requested scatter plot and returns TRUE
#'
#' @examples
#' plot_potential_energy(x=list("level:0"=0:12),y=0,home=0,unlist=TRUE,columns=c("semitone","potential_energy"),title="Tonic Potential Energy 0",include_names=FALSE)
#' plot_potential_energy(x=list("level:0"=0:12),y=12,home=12,unlist=TRUE,columns=c("semitone","potential_energy"),title="Octave Potential Energy 0",include_names=FALSE)
#'
#' @export
plot_potential_energy <- function(x,y,home,columns,unlist=FALSE,include_names=TRUE,title=NULL) {
  checkmate::assert(checkmate::check_list(x,types="integerish"))
  checkmate::assert_integerish(y)
  checkmate::assert_choice(home,c(0,12))
  checkmate::qassert(columns,"S2")
  checkmate::assert_logical(unlist)
  checkmate::assert_logical(include_names)
  checkmate::qassert(title,"S1")
  h = x
  if (unlist) { x = x %>% unlist}
  if (include_names) {
    n = names(x)
    l = list(x=x,name=n)
    h = purrr::pmap(l,potential_energy,y=y,home=home) %>% purrr::map_dfr(.f=dplyr::bind_rows)
  } else {
    h = purrr::map(x,potential_energy,y=y,home=home) %>% purrr::map_dfr(.f=dplyr::bind_rows)
  }
  plot(h[,columns],main=title)
  text(h[,columns],labels=h$intervallic_name,pos=1)
  if (include_names) {
    text(h[,columns],labels=h$name,pos=3)
  }
  TRUE
}

#' Homey Plot Potential Energy
#'
#' Provides scatter plots of musical potential energy of a note or chord with the homey theme.
#'
#' @param x A list of notes or chords expressed as an interval integers or vector of interval integers
#' @param y A list of notes or chords expressed as an interval integers or vector of interval integers
#' @param home The home pitch expressed an as interval integer
#' @param columns A vector of 2 column names to plot: semitone, affinity, brightness and magnitude
#' @param unlist=FALSE A logical TRUE or FALSE to plot points individually or as one point
#' @param include_names=TRUE Include the names from the list x on the plot
#' @param title=NULL An optional title for the plot
#' @return Generates the requested scatter plot and returns TRUE
#'
#' @examples
#' homey_plot_potential_energy(combn(0:12,2,simplify=FALSE),c(0,12),home=0,columns=c("brightness","affinity"),title="all 2 note chords in level 0 tonic home")
#' homey_plot_potential_energy(combn(0:12,2,simplify=FALSE),c(0,12),home=12,columns=c("brightness","affinity"),title="all 2 note chords in level 0 octave home")
#'
#' @export
homey_plot_potential_energy <- function(x,y,home,columns,unlist=FALSE,include_names=TRUE,symmetrical=TRUE,expansion_mult = 0.6,title=NULL) {
  checkmate::assert(checkmate::check_list(x,types="integerish"))
  checkmate::assert_choice(home,c(0,12))
  checkmate::qassert(columns,"S2")
  checkmate::assert_logical(unlist)
  checkmate::assert_logical(include_names)
  checkmate::assert_character(title,null.ok=TRUE)

  if (unlist) {x = x %>% unlist }
  h = x
  if (include_names) {
    n = names(x)
    l = list(x=x,name=n)
    h = purrr::pmap(l,potential_energy,y=y,home=home) %>% purrr::map_dfr(.f=dplyr::bind_rows)
  } else {
    h = purrr::map(x,potential_energy,y=y,home=home) %>% purrr::map_dfr(.f=dplyr::bind_rows)
  }

  colour_factor = colour_factor_homey(h)
  color_values = color_values_homey()
  p = h %>% ggplot2::ggplot(ggplot2::aes_string(x = columns[1], y = columns[2], colour=colour_factor)) +
    ggplot2::geom_point(ggplot2::aes(size=affinity)) +
    ggplot2::scale_size(guide="none") +
    ggplot2::scale_color_manual(values = color_values, guide="none") +
    ggplot2::ggtitle(title) +
    theme_homey()

  if (symmetrical) {
    p = p + ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = expansion_mult), limits=c((0-max(abs(h[columns[1]]))),(0+max(abs(h[columns[1]])))))
  } else {
    p = p + ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = expansion_mult))
  }
  if (include_names) {
    p = p + ggplot2::geom_label(ggplot2::aes(label=name),label.size = NA,fill=NA,vjust='bottom',hjust="outward",label.padding = ggplot2::unit(0.5, "lines"))
  } else {
    p = p + ggplot2::geom_label(ggplot2::aes(label=intervallic_name),label.size = NA,fill=NA,vjust='bottom',hjust="outward",label.padding = ggplot2::unit(0.5, "lines"))
  }
  p
}
