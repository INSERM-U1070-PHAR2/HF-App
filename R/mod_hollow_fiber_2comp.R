#' hollow_fiber_2compUI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_hollow_fiber_2comp_ui <- function(id){
  ns <- NS(id)
  tagList(fluidPage(
    tabsetPanel(
      id = "tabset2comp",
      type = "pills",
      tabPanel(
        "HF-Setup",
        fluidRow(
          mod_input_pump_ui(ns("input_pump_2comp")),
          mod_input_exp_system_ui(ns("input_exp_system_2comp")),
          mod_input_drug_charac_2comp_ui(ns("input_drug_charac_2comp")),
          mod_input_dosing_charac_2comp_ui(ns("input_dosing_charac_2comp"))
        ),
        fluidRow(
          column(
            12,
            align = "center",
            style = "padding-bottom:10px",
            mod_input_start_sim_button_ui(ns("input_start_sim_button_2comp"))
          )
        ),
        fluidRow(
          mod_parameter_table_2comp_ui(ns("parameter_table_2comp")),
          mod_experiment_diagram_2comp_ui(ns("experiment_diagram_2comp"))
        )
      ),
      tabPanel(
        "Simulations plotting",
        mod_graph_concentrations_2comp_ui(ns("graph_concentrations_2comp"))
      ),
      tabPanel("Simulation data",
               mod_download_simulated_data_ui(ns(
                 "download_simulated_data"
               )))
    )
  ))
}

#' hollow_fiber_2compServer Functions
#'
#' @noRd
mod_hollow_fiber_2comp_server <- function(id){
  moduleServer( id, function(input, output, session){

    ns <- session$ns
    library(zeallot) #used for the %<-% operator
    c(minPumpFlow, maxPumpFlow, stepPumpFlow) %<-% mod_input_pump_server("input_pump_2comp")
    c(Vcentral,
      debitCentralCartridge,
      Vcartridge,
      constantVolume) %<-% mod_input_exp_system_server("input_exp_system_2comp")
    c(drugNameA,
      drugNameB,
      initialConcentrationA,
      initialConcentrationB,
      lastTimePointHours,
      halfLifeHoursA,
      halfLifeHoursB) %<-% mod_input_drug_charac_2comp_server(
        "input_drug_charac_2comp",
        minPumpFlow,
        maxPumpFlow,
        stepPumpFlow,
        Vcentral,
        Vcartridge
      )
    c(admType,
      VinjectBolusA,
      dosingIntervalHoursBolusA,
      numberOfDosesBolusA,
      VinjectBolusB,
      VinjectBolusExtraB,
      dosingIntervalHoursBolusB,
      numberOfDosesBolusB,
      tinfuseHoursA,
      VinjectInfA,
      dosingIntervalHoursInfA,
      numberOfDosesInfA,
      tinfuseHoursB,
      VinjectInfB,
      VinjectInfExtraB,
      dosingIntervalHoursInfB,
      numberOfDosesInfB
    ) %<-% mod_input_dosing_charac_2comp_server("input_dosing_charac_2comp")

    simulateButton <-
      mod_input_start_sim_button_server("input_start_sim_button_2comp")

    parameterTable <-
      mod_parameter_table_2comp_server(
        "parameter_table_2comp",
        halfLifeHoursA,
        halfLifeHoursB,
        Vcentral,
        Vcartridge,
        drugNameA,
        drugNameB,
        initialConcentrationA,
        initialConcentrationB,
        lastTimePointHours,
        admType,
        VinjectBolusA,
        dosingIntervalHoursBolusA,
        numberOfDosesBolusA,
        VinjectBolusB,
        VinjectBolusExtraB,
        dosingIntervalHoursBolusB,
        numberOfDosesBolusB,
        tinfuseHoursA,
        VinjectInfA,
        dosingIntervalHoursInfA,
        numberOfDosesInfA,
        tinfuseHoursB,
        VinjectInfB,
        VinjectInfExtraB,
        dosingIntervalHoursInfB,
        numberOfDosesInfB,
        debitCentralCartridge,
        simulateButton
      )

    imgFilePath <- resourcePaths()['img']

    experimentDiagram <-
      mod_experiment_diagram_2comp_server(
        "experiment_diagram_2comp",
        parameterTable,
        admType,
        imgFilePath,
        simulateButton
      )
    defaultDataFilePath <- resourcePaths()['default_data']
    modelFilePath <- resourcePaths()['models']
    simulatedData <-
      mod_simulate_concentrations_2comp_server(
        "simulate_concentrations_2comp",
        defaultDataFilePath,
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
        simulateButton
      )


    mod_graph_concentrations_2comp_server(
      "graph_concentrations_2comp",
      simulatedData,
      lastTimePointHours,
      drugNameA,
      drugNameB,
      admType
    )

    mod_download_simulated_data_server("download_simulated_data",
                                       simulatedData)
  })

}

## To be copied in the UI
# mod_hollow_fiber_2_comp_ui("hollow_fiber_2_comp_1")

## To be copied in the server
# mod_hollow_fiber_2_comp_server("hollow_fiber_2_comp_1")
