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
#' @export
plot_harmony <- function(x,home=NULL,columns,unlist=FALSE,include_names=TRUE,title=NULL) {
  if (is.null(names(x))) {include_names=FALSE}
  checkmate::assert(checkmate::check_list(x,types="integerish"))
  checkmate::assert_choice(home,c(0,12),null.ok = TRUE)
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
#' @export
homey_plot_harmony <- function(x,home=NULL,columns,unlist=FALSE,include_names=TRUE,title=NULL,pascal_triangle=FALSE,repel_labels=FALSE,max_overlaps=Inf) {
  if (is.null(names(x))) {include_names=FALSE}
  checkmate::assert(checkmate::check_list(x,types="integerish"))
  checkmate::assert_choice(home,c(0,12),null.ok=TRUE)
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

  p = h %>% ggplot2::ggplot(ggplot2::aes_string(x = columns[1], y = columns[2], colour=colour_factor)) +
    ggplot2::geom_point() +
    ggplot2::scale_color_manual(values = color_values, guide="none") +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0.6), limits=c((0-max(abs(h[columns[1]]))),(0+max(abs(h[columns[1]]))))) +
    ggplot2::ggtitle(title) +
    theme_homey()
  if (pascal_triangle) {
    p = p + ggplot2::scale_y_continuous(breaks = numbers::pascal_triangle(6)[,3], minor_breaks=c(7))
  }
  if (repel_labels) {
    if (include_names) {
      p = p + ggrepel::geom_text_repel(ggplot2::aes(label=name), max.overlaps = max_overlaps)
    } else {
      p = p + ggrepel::geom_text_repel(ggplot2::aes(label=intervallic_name), max.overlaps = max_overlaps)
    }
  } else {
    if (include_names) {
      p = p + ggplot2::geom_label(ggplot2::aes(label=name),label.size = NA,fill=NA,vjust='bottom',hjust="outward",label.padding = ggplot2::unit(0.3, "lines"))
    } else {
      p = p + ggplot2::geom_label(ggplot2::aes(label=intervallic_name),label.size = NA,fill=NA,vjust='bottom',hjust="outward",label.padding = ggplot2::unit(0.3, "lines"))
    }
  }
  p
}
