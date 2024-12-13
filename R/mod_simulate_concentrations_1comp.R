#' simulate_concentrations_1comp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_simulate_concentrations_1comp_ui <- function(id){
  ns <- NS(id)
  tagList(

  )
}

#' simulate_concentrations_1comp Server Functions
#'
#' @noRd
mod_simulate_concentrations_1comp_server <- function(id,
                                                      modelFilePath,
                                                      halfLifeHours,
                                                      Vcentral,
                                                      Vcartridge,
                                                      initialConcentration,
                                                      lastTimePointHours,
                                                      drugName,
                                                      VinjectBolus,
                                                      dosingIntervalHoursBolus,
                                                      numberOfDosesBolus,
                                                      tinfuseHours,
                                                      VinjectInf,
                                                      dosingIntervalHoursInf,
                                                      numberOfDosesInf,
                                                      admType,
                                                      constantVolume,
                                                      debitCentralCartridge,
                                                      css,
                                                      Cinfusemaintenance,
                                                      simulateButton){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    values <- reactiveValues()
    model <- mrgsolve::mread(file.path(modelFilePath,"HollowFiber1Comp-Vadd"))
    observeEvent(simulateButton(),
            ({
              values$simulatedData <- HollowFibre1CompSimData(
                model,
                halfLifeHours(),
                Vcentral(),
                Vcartridge(),
                initialConcentration(),
                lastTimePointHours(),
                drugName(),
                VinjectBolus(),
                dosingIntervalHoursBolus(),
                numberOfDosesBolus(),
                tinfuseHours(),
                VinjectInf(),
                dosingIntervalHoursInf(),
                numberOfDosesInf(),
                admType(),
                constantVolume(),
                debitCentralCartridge(),
                css(),
                Cinfusemaintenance()
              )
            })
    )
    return(simulatedData = reactive({
      values$simulatedData
    }))





  })
}

## To be copied in the UI
# mod_simulate_concentrations_1comp_ui("simulate_concentrations_1comp_1")

## To be copied in the server
# mod_simulate_concentrations_1comp_server("simulate_concentrations_1comp_1")
