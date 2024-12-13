#' input_start_sim_button UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_input_start_sim_button_ui <- function(id) {
  ns <- NS(id)
  tagList(actionButton(ns("simulateButton"),
                       "Simulate HF",
                       style = "font-size:150%"))
}

#' input_start_sim_button Server Functions
#'
#' @noRd
mod_input_start_sim_button_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    return(simulateButton = reactive({
      input$simulateButton
    }))
  })
}

## To be copied in the UI
# mod_input_start_sim_button_ui("input_start_sim_button_1")

## To be copied in the server
# mod_input_start_sim_button_server("input_start_sim_button_1")
