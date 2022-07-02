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
  if (is.null(names(x))) {include_names=FALSE}
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

