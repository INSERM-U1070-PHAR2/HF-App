#' simulate_concentrations_1comp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_simulate_concentrations_1comp_abs_ui <- function(id){
  ns <- NS(id)
  tagList(

  )
}

#' simulate_concentrations_1comp Server Functions
#'
#' @noRd
mod_simulate_concentrations_1comp_abs_server <- function(id,
                                                         defaultDataFilePath,
                                                      modelFilePath,
                                                      halfLifeHours,
                                                      Vcentral,
                                                      Vcartridge,
                                                      initialConcentration,
                                                      lastTimePointHours,
                                                      drugName,
                                                      constantVolume,
                                                      debitCentralCartridge,
                                                      nDoses,
                                                      ka,
                                                      f_avail,
                                                      nInfusions,
                                                      dosingIntervalHoursAbs,
                                                      CinfusionMin,
                                                      CinfusionMax,
                                                      CinfusionStep,
                                                      minInfusionVolume,
                                                      simulateButton){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    defaultData <- readr::read_csv(file.path(defaultDataFilePath,"default_simulated_data_1comp_absorption.csv"))
    values <- reactiveValues(simulatedData = defaultData)
    model <- mrgsolve::mread(file.path(modelFilePath,"HollowFiber1Comp-Vadd-Abs"))
    observeEvent(simulateButton(),
            ({
              values$simulatedData <- HollowFibre1CompAbsSimData(
                model = model,
                halfLifeHours = halfLifeHours(),
                Vcentral = Vcentral(),
                Vcartridge =  Vcartridge(),
                initial_concentration = initialConcentration(),
                lastTimePointHours = lastTimePointHours(),
                drugName = drugName(),
                constantVolume = constantVolume(),
                debit_central_cartridge = debitCentralCartridge(),
                nDoses = nDoses(),
                ka = ka(),
                f_avail = f_avail(),
                nInfusions = nInfusions(),
                dosingIntervalHoursAbs = dosingIntervalHoursAbs(),
                CinfusionMin = CinfusionMin(),
                CinfusionMax = CinfusionMax(),
                CinfusionStep = CinfusionStep(),
                minInfusionVolume = minInfusionVolume()
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
