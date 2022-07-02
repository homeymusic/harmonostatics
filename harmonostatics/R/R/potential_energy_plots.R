plot_potential_energy <- function(x,y,home,columns,unlist=FALSE,include_names=TRUE,names=NULL,title=NULL) {
  if (is.null(names(x))) {include_names=FALSE}
  checkmate::assert(checkmate::check_list(x,types="integerish"))
  checkmate::assert_integerish(y)
  checkmate::assert_choice(home,c(0,12))
  checkmate::qassert(columns,"S2")
  checkmate::assert_logical(unlist)
  checkmate::assert_logical(include_names)
  checkmate::assert_character(names,null.ok=TRUE)
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

