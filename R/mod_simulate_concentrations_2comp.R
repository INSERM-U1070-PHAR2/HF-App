#' simulate_concentrations_2comp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_simulate_concentrations_2comp_ui <- function(id){
  ns <- NS(id)
  tagList(

  )
}

#' simulate_concentrations_2comp Server Functions
#'
#' @noRd
mod_simulate_concentrations_2comp_server <- function(id,
                                                     modelFilePath,
                                                     halfLifeHoursA,
                                                     halfLifeHoursB,
                                                     Vcentral,
                                                     Vcartridge,
                                                     initialConcentrationA,
                                                     initialConcentrationB,
                                                     lastTimePointHours,
                                                     VinjectBolusA,
                                                     VinjectBolusB,
                                                     VinjectBolusExtraB,
                                                     dosingIntervalHoursBolusA,
                                                     dosingIntervalHoursBolusB,
                                                     numberOfDosesBolusA,
                                                     numberOfDosesBolusB,
                                                     drugNameA,
                                                     drugNameB,
                                                     admType,
                                                     tinfuseHoursA,
                                                     tinfuseHoursB,
                                                     VinjectInfA,
                                                     VinjectInfB,
                                                     VinjectInfExtraB,
                                                     dosingIntervalHoursInfA,
                                                     dosingIntervalHoursInfB,
                                                     numberOfDosesInfA,
                                                     numberOfDosesInfB,
                                                     constantVolume,
                                                     debitCentralCartridge,
                                                     simulateButton){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    values <- reactiveValues()
    model <- mrgsolve::mread(file.path(modelFilePath,"HollowFiber2Comp-Vadd"))
    observeEvent(simulateButton(),
                 ({
                   values$simulatedData <- HollowFibre2CompSimData(
                     model,
                     as.numeric(halfLifeHoursA()),
                     as.numeric(halfLifeHoursB()),
                     Vcentral(),
                     Vcartridge(),
                     initialConcentrationA(),
                     initialConcentrationB(),
                     lastTimePointHours(),
                     VinjectBolusA(),
                     VinjectBolusB(),
                     VinjectBolusExtraB(),
                     dosingIntervalHoursBolusA(),
                     dosingIntervalHoursBolusB(),
                     numberOfDosesBolusA(),
                     numberOfDosesBolusB(),
                     drugNameA(),
                     drugNameB(),
                     admType(),
                     tinfuseHoursA(),
                     tinfuseHoursB(),
                     VinjectInfA(),
                     VinjectInfB(),
                     VinjectInfExtraB(),
                     dosingIntervalHoursInfA(),
                     dosingIntervalHoursInfB(),
                     numberOfDosesInfA(),
                     numberOfDosesInfB(),
                     constantVolume(),
                     debitCentralCartridge()
                   )
                 })
    )
    return(simulatedData = reactive({
      values$simulatedData
    }))
  })
}

## To be copied in the UI
# mod_simulate_concentrations_2comp_ui("simulate_concentrations_2comp_1")

## To be copied in the server
# mod_simulate_concentrations_2comp_server("simulate_concentrations_2comp_1")
