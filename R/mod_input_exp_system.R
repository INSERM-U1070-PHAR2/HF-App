#' input_exp_system UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_input_exp_system_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinydashboard::box(
      title = "Experimental system characteristics",
      status = "primary",
      solidHeader = TRUE,
      collapsible = TRUE,
      width = 3,

      numericInput(
        ns("Vcentral"),
        "Central volume (mL)",
        200,
        min = 0,
        step = 0.01
      ),
      numericInput(
        ns("debitCentralCartridge"),
        "Flow rate central to cartridge (mL/min)",
        120,
        min = 0,
        step = 0.01
      ),
      numericInput(
        ns("Vcartridge"),
        "Cartridge volume (mL)",
        50,
        min = 0,
        step = 0.01
      ),
      checkboxInput(ns("constantVolume"),
                    "Is volume constant ?",
                    value = T)
    )
  )
}

#' input_exp_system Server Functions
#'
#' @return list with the following components
#' \describe{
#'  \item{Vcentral} reactive number indicating the volume of the central bottle in mL
#'  \item{debitCentralCartridge} reactive number indicating the pump flow from central bottle to cartridge in mL/min
#'  \item{Vcartridge} reactive number indicating cartridge volume in mL
#'  \item{constantVolume} reactive binary indicating whether the central bottle volume should be assumed to be constant in simulations
#'  }
#'
#' @noRd
mod_input_exp_system_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    return(list(
      Vcentral = reactive({
        input$Vcentral
      }),
      debitCentralCartridge = reactive({
        input$debitCentralCartridge
      }),
      Vcartridge = reactive({
        input$Vcartridge
      }),
      constantVolume = reactive({
        input$constantVolume
      })
    ))

  })
}

## To be copied in the UI
# mod_input_exp_system_ui("input_exp_system_1")

## To be copied in the server
# mod_input_exp_system_server("input_exp_system_1")
