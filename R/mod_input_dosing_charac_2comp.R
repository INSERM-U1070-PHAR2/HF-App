#' input_dosing_charac_2comp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_input_dosing_charac_2comp_ui <- function(id){
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
        choices = c("Bolus", "Infusion"),
        selected = "Bolus"
      ),
      conditionalPanel(
        condition = "input.admType== 'Bolus'",

        numericInput(
          ns("VinjectBolusA"),
          "Bolus volume of drug A into central (mL)",
          10,
          min = 0,
          step = 0.01
        ),
        numericInput(
          ns("dosingIntervalHoursBolusA"),
          "Dosing interval for drug A (hours)",
          12,
          min = 0,
          step = 0.01
        ),
        numericInput(
          ns("numberOfDosesBolusA"),
          "Total number of doses of drug A",
          2,
          min = 1,
          step = 1
        ),
        numericInput(
          ns("VinjectBolusB"),
          "Bolus volume for drug B into central (mL)",
          10,
          min = 0,
          step = 0.01
        ),
        numericInput(
          ns("VinjectBolusExtraB"),
          "Bolus volume for drug B into extra (mL)",
          10,
          min = 0,
          step = 0.01
        ),
        numericInput(
          ns("dosingIntervalHoursBolusB"),
          "Dosing interval for drug B (hours)",
          12,
          min = 0,
          step = 0.01
        ),
        numericInput(
          ns("numberOfDosesBolusB"),
          "Total number of doses of drug B",
          2,
          min = 1,
          step = 1
        ),
        ns = ns),
      conditionalPanel(
        condition = "input.admType== 'Infusion'",

        numericInput(
          ns("tinfuseHoursA"),
          "Infusion duration of drug A in central compartment (hours)",
          1,
          min = 0,
          step = 0.01
        ),
        numericInput(
          ns("VinjectInfA"),
          "Infusion volume for drug A in central compartment (mL)",
          10,
          min = 0,
          step = 0.01
        ),
        numericInput(
          ns("dosingIntervalHoursInfA"),
          "Dosing interval for drug A (hours)",
          12,
          min = 0,
          step = 0.01
        ),
        numericInput(
          ns("numberOfDosesInfB"),
          "Total number of doses of drug B",
          2,
          min = 1,
          step = 1
        ),
        numericInput(
          ns("tinfuseHoursB"),
          "Infusion duration of drug B in central and extra compartment (hours)",
          1,
          min = 0,
          step = 0.01
        ),
        numericInput(
          ns("VinjectInfB"),
          "Infusion volume for drug B in central compartment (mL)",
          10,
          min = 0,
          step = 0.01
        ),
        numericInput(
          ns("VinjectInfExtraB"),
          "Infusion volume for drug B in extra compartment (mL)",
          10,
          min = 0,
          step = 0.01
        ),
        numericInput(
          ns("dosingIntervalHoursInfB"),
          "Dosing interval for drug B (hours)",
          12,
          min = 0,
          step = 0.01
        ),
        numericInput(
          ns("numberOfDosesInfB"),
          "Total number of doses of drug B",
          2,
          min = 1,
          step = 1
        ),
        ns = ns
      )
    )
  )
}

#' input_dosing_charac_2comp Server Functions
#'
#' @noRd
mod_input_dosing_charac_2comp_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    return(list(
      admType = reactive({
        input$admType
      }),
      VinjectBolusA = reactive({
        input$VinjectBolusA
      }),
      dosingIntervalHoursBolusA = reactive({
        input$dosingIntervalHoursBolusA
      }),
      numberOfDosesBolusA = reactive({
        input$numberOfDosesBolusA
      }),
      VinjectBolusB = reactive({
        input$VinjectBolusB
      }),
      VinjectBolusExtraB = reactive({
        input$VinjectBolusExtraB
      }),
      dosingIntervalHoursBolusB = reactive({
        input$dosingIntervalHoursBolusB
      }),
      numberOfDosesBolusB = reactive({
        input$numberOfDosesBolusB
      }),
      tinfuseHoursA = reactive({
        input$tinfuseHoursA
      }),
      VinjectInfA = reactive({
        input$VinjectInfA
      }),
      dosingIntervalHoursInfA = reactive({
        input$dosingIntervalHoursInfA
      }),
      numberOfDosesInfA = reactive({
        input$numberOfDosesInfA
      }),
      tinfuseHoursB = reactive({
        input$tinfuseHoursB
      }),
      VinjectInfB = reactive({
        input$VinjectInfB
      }),
      VinjectInfExtraB = reactive({
        input$VinjectInfExtraB
      }),
      dosingIntervalHoursInfB = reactive({
        input$dosingIntervalHoursInfB
      }),
      numberOfDosesInfB = reactive({
        input$numberOfDosesInfB
      })
    ))
  })
}

## To be copied in the UI
# mod_input_dosing_charac_2comp_ui("input_dosing_charac_2comp_1")

## To be copied in the server
# mod_input_dosing_charac_2comp_server("input_dosing_charac_2comp_1")
