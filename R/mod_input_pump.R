#' input_pump UI Function
#'
#' @description Module that takes pump characteristics input from users.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom shinydashboard box
mod_input_pump_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinydashboard::box(
      title = "Pump characteristics",
      status = "primary",
      solidHeader = TRUE,
      collapsible = TRUE,
      width = 3,
      numericInput(
        ns("minPumpFlow"),
        "Minimum Pump Flow (mL/min)",
        0.4,
        min = 0,
        step = 0.1
      ),
      numericInput(
        ns("maxPumpFlow"),
        "Maximum Pump Flow (mL/min)",
        150,
        min = 0,
        step = 0.1
      ),
      numericInput(
        ns("stepPumpFlow"),
        "Step of pump (mL/min)",
        0.1,
        min = 0,
        step = 0.1
      )
    )
  )
}

#' input_pump Server Functions
#'
#'#' @return list with the following components
#' \describe{
#'  \item{minPumpFlow} reactive number indicating the minimal pump flow in mL/min
#'  \item{maxPumpFlow} reactive number indicating the maximal pump flow in mL/min
#'  \item{stepPumpFlow} reactive number indicating the step between possible pump flows in mL/min}
#'
#' @noRd
mod_input_pump_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
return(
  list(
    minPumpFlow = reactive({input$minPumpFlow}),
    maxPumpFlow = reactive({input$maxPumpFlow}),
    stepPumpFlow = reactive({input$stepPumpFlow})
  )
)
  })
}

## To be copied in the UI
# mod_input_pump_ui("input_pump_1")

## To be copied in the server
# mod_input_pump_server("input_pump_1")
