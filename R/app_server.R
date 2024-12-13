#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  mod_hollow_fiber_1comp_server("hf_1comp")
  mod_hollow_fiber_2comp_server("hf_2comp")



}
