
#' Shiny interface for effectLite
#' 
#' This function calls a shiny interface for effectLite.
#' 
#' @param launch.browser Option will be passed on to \code{\link[shiny]{runApp}}
#' @export
effectLiteGUI <- function(launch.browser=TRUE){
  shiny::runApp(system.file('elrshiny', package='EffectLiteR'),
                launch.browser=launch.browser)
}


#' Shiny interface for elrEffects
#' 
#' This function calls a shiny interface for elrEffects.
#' 
#' @param launch.browser Option will be passed on to \code{\link[shiny]{runApp}}
#' @export
elrEffectsGUI <- function(launch.browser=TRUE){
  tmp <- DT::coerceValue(100,1L) ## only needed to avoid warning in check --as cran
  shiny::runApp(system.file('elreffectsshiny', package='EffectLiteR'),
                launch.browser=launch.browser)
}

