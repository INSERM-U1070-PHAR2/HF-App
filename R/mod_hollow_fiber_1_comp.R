#' hollow_fiber_1comp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_hollow_fiber_1comp_ui <- function(id) {
  ns <- NS(id)
  tagList(fluidPage(
    tabsetPanel(
      id = "tabset1comp",
      type = "pills",
      tabPanel(
        "HF-Setup",
        fluidRow(
          mod_input_pump_ui(ns("input_pump_1comp")),
          mod_input_exp_system_ui(ns("input_exp_system_1comp")),
          mod_input_drug_charac_ui(ns("input_drug_charac_1comp")),
          mod_input_dosing_charac_ui(ns("input_dosing_charac_1comp"))
        ),
        fluidRow(
          column(
            12,
            align = "center",
            style = "padding-bottom:10px",
            mod_input_start_sim_button_ui(ns("input_start_sim_button_1comp"))
          )
        ),
        fluidRow(
          mod_parameter_table_1comp_ui(ns("parameter_table_1comp")),
          mod_experiment_diagram_1comp_ui(ns("experiment_diagram_1comp"))
        )
      ),
      tabPanel(
        "Simulations plotting",
        mod_graph_concentrations_1comp_ui(ns("graph_concentrations_1comp"))
      ),
      tabPanel("Simulation data",
               mod_download_simulated_data_ui(ns(
                 "download_simulated_data"
               )))
    )
  ))
}

#' hollow_fiber_1comp Server Functions
#'
#' @noRd
mod_hollow_fiber_1comp_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    library(zeallot) #used for the %<-% operator
    c(minPumpFlow, maxPumpFlow, stepPumpFlow) %<-% mod_input_pump_server("input_pump_1comp")
    c(Vcentral,
      debitCentralCartridge,
      Vcartridge,
      constantVolume) %<-% mod_input_exp_system_server("input_exp_system_1comp")
    c(drugName,
      initialConcentration,
      lastTimePointHours,
      halfLifeHours) %<-% mod_input_drug_charac_server(
        "input_drug_charac_1comp",
        minPumpFlow,
        maxPumpFlow,
        stepPumpFlow,
        Vcentral,
        Vcartridge
      )
    c(
      admType,
      VinjectBolus,
      dosingIntervalHoursBolus,
      numberOfDosesBolus,
      tinfuseHours,
      VinjectInf,
      dosingIntervalHoursInf,
      numberOfDosesInf,
      css,
      VinjectLoadingDose,
      minPumpFlowInfuse,
      maxPumpFlowInfuse,
      stepPumpFlowInfuse,
      cInfuseMaintenance
    ) %<-% mod_input_dosing_charac_server("input_dosing_charac_1comp",
                                          Vcentral,
                                          Vcartridge)

    simulateButton <-
      mod_input_start_sim_button_server("input_start_sim_button_1comp")

    parameterTable <-
      mod_parameter_table_1comp_server(
        "parameter_table_1comp",
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
        debitCentralCartridge,
        css,
        Cinfusemaintenance,
        VinjectLoadingDose,
        simulateButton
      )

    imgFilePath <- resourcePaths()['img']

    experimentDiagram <-
      mod_experiment_diagram_1comp_server(
        "experiment_diagram_1comp",
        parameterTable,
        admType,
        drugName,
        imgFilePath,
        simulateButton
      )

    modelFilePath <- resourcePaths()['models']
    simulatedData <-
      mod_simulate_concentrations_1comp_server(
        "simulate_concentrations_1comp",
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
        simulateButton
      )


    mod_graph_concentrations_1comp_server(
      "graph_concentrations_1comp",
      simulatedData,
      lastTimePointHours,
      dosingIntervalHoursBolus,
      dosingIntervalHoursInf,
      admType,
      drugName,
      simulateButton
    )

    mod_download_simulated_data_server("download_simulated_data",
                                       simulatedData)
  })
}

## To be copied in the UI
# mod_hollow_fiber_1comp_ui("hollow_fiber_1comp_1")

## To be copied in the server
# mod_hollow_fiber_1comp_server("hollow_fiber_1comp_1")
