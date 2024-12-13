#' input_drug_charac UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_input_drug_charac_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinydashboard::box(
      title = "Drug charateristics",
      status = "primary",
      solidHeader = TRUE,
      collapsible = TRUE,
      width = 3,
      textInput(ns("drugName"), "Name of the drug", "Drug"),
      selectInput(ns("halfLifeHours"),
                  "Half life of drug (hours)",
                  choices = NULL),
      numericInput(
        ns("initialConcentration"),
        "Peak concentration after 1st dose (mg/L)",
        2
        ,
        min = 0,
        step = 0.01
      ),
      numericInput(
        ns("lastTimePointHours"),
        "Experiment duration (hours)",
        24,
        min = 0,
        step = 0.01
      )
    )
  )
}

#' input_drug_charac Server Functions
#'
#' @return list with the following components
#' \describe{
#'  \item{drugName} reactive character indicating the name of the drug
#'  \item{initialConcentration} reactive number the desired concentration at time = 0 in mg/L
#'  \item{lastTimePointhours} reactive number indicating the time at which the experiment will be stopped in hours
#'  \item{halfLifeHours} reactive number indiating the selected elimination half-life in hours
#'}
#' @noRd
mod_input_drug_charac_server <- function(id,
                                         minPumpFlow,
                                         maxPumpFlow,
                                         stepPumpFlow,
                                         Vcentral,
                                         Vcartridge) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
# Compute possible half lives whic hdepend on pump characteristics and volumes of bottles
   possibleHalfLives <- reactive({calculatePossibleHalfLives(minPumpFlow(),
                                                    maxPumpFlow(),
                                                    stepPumpFlow(),
                                                    Vcentral(),
                                                    Vcartridge(),
                                                    roundingDigits = 2)})
#Update the possible half lives when there is a change in the pump characteristics and/or volumes
    observe({updateSelectInput(session,
                      "halfLifeHours",
                      choices = possibleHalfLives())})
    return(
      list(
        drugName = reactive({
          input$drugName
        }),
        initialConcentration = reactive({
          input$initialConcentration
        }),
        lastTimePointhours = reactive({
          input$lastTimePointHours
        }),
        halfLifeHours = reactive({
          as.numeric(input$halfLifeHours)
        })
      )
    )

  })
}

## To be copied in the UI
# mod_input_drug_charac_ui("input_drug_charac_1")

## To be copied in the server
# mod_input_drug_charac_server("input_drug_charac_1")
