#' input_dosing_charac UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_input_dosing_charac_ui <- function(id){
  ns <- NS(id)
  tagList(
    box(
      title = "Dosing characteristics",
      status = "primary",
      solidHeader = TRUE,
      collapsible = TRUE,
      width = 3,
      radioButtons(
        ns("admType"),
        "Type of injection",
        choices = c("Bolus", "Infusion","Loading dose + Infusion"),
        selected = "Bolus"
      ),
      conditionalPanel(
        condition = "input.admType== 'Bolus'",

        numericInput(
          ns("VinjectBolus"),
          "Bolus volume (mL)",
          10,
          min = 0,
          step = 0.01
        ),
        numericInput(
          ns("dosingIntervalHoursBolus"),
          "Dosing interval (hours)",
          12,
          min = 0,
          step = 0.01
        ),
        numericInput(
          ns("numberOfDosesBolus"),
          "Total number of doses",
          2,
          min = 1,
          step = 1
        ),
        ns = ns),
      conditionalPanel(
        condition = "input.admType== 'Infusion'",

        numericInput(
          ns("tinfuseHours"),
          "Infusion duration (hours)",
          1,
          min = 0,
          step = 0.01
        ),
        numericInput(
          ns("VinjectInf"),
          "Infusion volume (mL)",
          10,
          min = 0,
          step = 0.01
        ),
        numericInput(
          ns("dosingIntervalHoursInf"),
          "Dosing interval (hours)",
          12,
          min = 0,
          step = 0.01
        ),
        numericInput(
          ns("numberOfDosesInf"),
          "Total number of doses",
          2,
          min = 1,
          step = 1
        ),
        ns = ns
      ),
      conditionalPanel(
        condition = "input.admType== 'Loading dose + Infusion'",
        numericInput(
          ns("css"),
          "Steady state concentration (mg/L)",
          1,
          min = 0,
          step = 0.01
        ),
        numericInput(
          ns("VinjectLoadingDose"),
          "Loading dose volume (mL)",
          10,
          min = 0,
          step = 0.01
        ),
        numericInput(
          ns("minPumpFlowInfuse"),
          "Minimum Infusion Flow (mL/h)",
          0.1,
          min = 0,
          step = 0.1
        ),
        numericInput(
          ns("maxPumpFlowInfuse"),
          "Maximum Infusion Flow (mL/h)",
          400,
          min = 0,
          step = 0.1
        ),
        numericInput(
          ns("stepPumpFlowInfuse"),
          "Step of Infusion (mL/h)",
          0.1,
          min = 0,
          step = 0.1
        ),
        selectInput(ns("cInfuseMaintenance"),
                    "Infusion concentration (mg/L)",
                    choices = NULL),
        ns = ns
      )
    )
  )
}

#' input_dosing_charac Server Functions
#'
#' @return list with the following components
#' \describe{
#'  \item{admType} reactive character indicating the type of administration
#'  \item{VinjectBolus} reactive number indicating the volume of to be injected via bolus in mL
#'  \item{dosingIntervalHoursBolus} reactive number indicating dosing interval for bolus injection in hours
#'  \item{numberOfDosesBolus} reactive number indicating the total number of bolus doses planned for the experiment
#'  \item{tinfuseHours} reactive number indicating the infusion duration in hours
#'  \item{VinjectInf} reactive number indicating the volume to be injected via infusion in mL
#'  \item{dosingIntervalHoursInf} reactive number indicating the dosing interval for infusions in hours
#'  \item{numberOfDosesInf} reactive number indicating the total number of infusion doses planned for the experiment
#'  \item{css} reactive number indicating the target concentration at steady state in mg/L
#'  \item{VinjectLoadingDose} reactive number indicating the volume to be injected by loading dose in mL
#'  \item{minPumpFlowInfuse} reactive number indicating the minimal flow of the infusion pump in mL/h
#'  \item{maxPumpFlowInfuse} reactive number indicating the maximal flow of the infusion pump in mL/h
#'  \item{stepPumpFlowInfuse} reactive number indicating the step of flows of the infusion pump in mL/h
#'  }
#' @noRd
mod_input_dosing_charac_server <- function(id,
                                           Vcentral,
                                           Vcartridge){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Compute possible half lives whic hdepend on pump characteristics and volumes of bottles
    possibleMaintenanceConcentration <-
      reactive({
        calculatePossibleMaintenanceConcentration(
          input$minPumpFlowInfuse,
          input$maxPumpFlowInfuse,
          input$stepPumpFlowInfuse,
          Vcentral(),
          Vcartridge(),
          input$css,
          roundingDigits = 2
        )
      })
    #Update the possible half lives when there is a change in the pump characteristics and/or volumes
    observe({updateSelectInput(session,
                               "cInfuseMaintenance",
                               choices = possibleMaintenanceConcentration())})

    return(list(
      admType = reactive({
        input$admType
      }),
      VinjectBolus = reactive({
        input$VinjectBolus
      }),
      dosingIntervalHoursBolus = reactive({
        input$dosingIntervalHoursBolus
      }),
      numberOfDosesBolus = reactive({
        input$numberOfDosesBolus
      }),
      tinfuseHours = reactive({
        input$tinfuseHours
      }),
      VinjectInf = reactive({
        input$VinjectInf
      }),
      dosingIntervalHoursInf = reactive({
        input$dosingIntervalHoursInf
      }),
      numberOfDosesInf = reactive({
        input$numberOfDosesInf
      }),
      css = reactive({
        input$css
      }),
      VinjectLoadingDose = reactive({
        input$VinjectLoadingDose
      }),
      minPumpFlowInfuse = reactive({
        input$minPumpFlowInfuse
      }),
      maxPumpFlowInfuse = reactive({
        input$maxPumpFlowInfuse
      }),
      stepPumpFlowInfuse = reactive({
        input$stepPumpFlowInfuse
      }),
      cInfuseMaintenance = reactive({
        as.numeric(input$cInfuseMaintenance)
      })
    ))

  })
}

## To be copied in the UI
# mod_input_dosing_charac_ui("input_dosing_charac_1")

## To be copied in the server
# mod_input_dosing_charac_server("input_dosing_charac_1")
