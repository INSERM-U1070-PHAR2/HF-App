#' input_drug_charac_2comp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_input_drug_charac_2comp_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinydashboard::box(
      title = "Drug charateristics",
      status = "primary",
      solidHeader = TRUE,
      collapsible = TRUE,
      width = 3,
      textInput(ns("drugNameA"), "Name of drug A", "DrugA"),
      selectInput(ns("halfLifeHoursA"),
                  "Half life of drug A (hours)",
                  choices = NULL),
      numericInput(
        ns("initialConcentrationA"),
        "Peak concentration after 1st dose of drug A (mg/L)",
        2
        ,
        min = 0,
        step = 0.01
      ),
      textInput(ns("drugNameB"), "Name of drug B", "DrugB"),
      selectInput(ns("halfLifeHoursB"),
                  "Half life of drug B (hours)",
                  choices = NULL),
      numericInput(
        ns("initialConcentrationB"),
        "Peak concentration after 1st dose of drug B (mg/L)",
        0.8
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

#' input_drug_charac_2comp Server Functions
#'
#' @noRd
mod_input_drug_charac_2comp_server <- function(id,
                                               minPumpFlow,
                                               maxPumpFlow,
                                               stepPumpFlow,
                                               Vcentral,
                                               Vcartridge){
  moduleServer( id, function(input, output, session){
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
                               "halfLifeHoursA",
                               choices = possibleHalfLives())})
    observe({updateSelectInput(session,
                               "halfLifeHoursB",
                               choices = possibleHalfLives())})
    return(
      list(
        drugNameA = reactive({
          input$drugNameA
        }),
        drugNameB = reactive({
          input$drugNameB
        }),
        initialConcentrationA = reactive({
          input$initialConcentrationA
        }),
        initialConcentrationB = reactive({
          input$initialConcentrationB
        }),
        lastTimePointhours = reactive({
          input$lastTimePointHours
        }),
        halfLifeHoursA = reactive({
          as.numeric(input$halfLifeHoursA)
        }),
        halfLifeHoursB = reactive({
          as.numeric(input$halfLifeHoursB)
        })
      )
    )

  })
}

## To be copied in the UI
# mod_input_drug_charac_2comp_ui("input_drug_charac_2comp_1")

## To be copied in the server
# mod_input_drug_charac_2comp_server("input_drug_charac_2comp_1")
