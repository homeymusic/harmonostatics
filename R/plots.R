#' Plot Harmony
#'
#' Provides scatter plots of musical harmony metrics of a note or chord.
#'
#' @param x A list of notes or chords expressed as an interval integers or vector of interval integers
#' @param home The home pitch expressed an as interval integer
#' @param columns A vector of 2 column names to plot: semitone, affinity, brightness and magnitude
#' @param unlist A logical TRUE or FALSE to plot points individually or as one point
#' @param include_names Include the names from the list x on the plot
#' @param title An optional title for the plot
#' @return Generates the requested scatter plot and returns TRUE
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
#' @param unlist A logical TRUE or FALSE to plot points individually or as one point
#' @param include_names Include the names from the list x on the plot
#' @param title An optional title for the plot
#' @param pascal_triangle Plot with triangular numbers as vertical gridlines
#' @param repel_labels Space labels apart
#' @param max_overlaps for repel labels
#' @param expansion_mult add add padding horizontally, 0.6 is default
#' @return Generates the requested scatter plot and returns TRUE
#'
#' @export
homey_plot_harmony <- function(x,home=NULL,columns,unlist=FALSE,include_names=TRUE,title=NULL,pascal_triangle=FALSE,repel_labels=FALSE,max_overlaps=Inf,expansion_mult = 0.6) {
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
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = expansion_mult), limits=c((0-max(abs(h[columns[1]]))),(0+max(abs(h[columns[1]]))))) +
    ggplot2::ggtitle(title) +
    theme_homey()
  if (pascal_triangle) {
    p = p + ggplot2::scale_y_continuous(breaks = numbers::pascal_triangle(6)[,3], minor_breaks=c(7))
  }
  if (repel_labels) {
    if (include_names) {
      p = p + ggrepel::geom_text_repel(ggplot2::aes(label=.data$name), max.overlaps = max_overlaps, segment.color = NA)
    } else {
      p = p + ggrepel::geom_text_repel(ggplot2::aes(label=.data$intervallic_name), max.overlaps = max_overlaps, segment.color = NA)
    }
  } else {
    if (include_names) {
      p = p + ggplot2::geom_label(ggplot2::aes(label=.data$name),label.size = NA,fill=NA,vjust='bottom',hjust="outward",label.padding = ggplot2::unit(0.3, "lines"))
    } else {
      p = p + ggplot2::geom_label(ggplot2::aes(label=.data$intervallic_name),label.size = NA,fill=NA,vjust='bottom',hjust="outward",label.padding = ggplot2::unit(0.3, "lines"))
    }
  }
  p
}
#' Plot Potential Energy
#'
#' Provides scatter plots of potential energy between two notes or chords
#'
#' @param x A list of notes or chords expressed as an interval integers or vector of interval integers
#' @param y A list of notes or chords expressed as an interval integers or vector of interval integers
#' @param home The home pitch expressed an as interval integer
#' @param columns A vector of 2 column names to plot: semitone, affinity, brightness and magnitude
#' @param unlist A logical TRUE or FALSE to plot points individually or as one point
#' @param include_names Include the names from the list x on the plot
#' @param title An optional title for the plot
#' @return Generates the requested scatter plot and returns TRUE
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
#' @param unlist A logical TRUE or FALSE to plot points individually or as one point
#' @param include_names Include the names from the list x on the plot
#' @param symmetrical Center the plot horizontally
#' @param expansion_mult Leave space for the labels within the chart
#' @param title An optional title for the plot
#' @return Generates the requested scatter plot and returns TRUE
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
    p = p + ggplot2::geom_label(ggplot2::aes(label=.data$name),label.size = NA,fill=NA,vjust='bottom',hjust="outward",label.padding = ggplot2::unit(0.5, "lines"))
  } else {
    p = p + ggplot2::geom_label(ggplot2::aes(label=.data$intervallic_name),label.size = NA,fill=NA,vjust='bottom',hjust="outward",label.padding = ggplot2::unit(0.5, "lines"))
  }
  p
}
colors_homey <- function() {
  list(
    'background' = '#664433',
    'foreground' = '#F3DDAB',
    'minor' = '#ABDAF3',
    'neutral' = '#FF5500',
    'major' = '#F3A904'
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
