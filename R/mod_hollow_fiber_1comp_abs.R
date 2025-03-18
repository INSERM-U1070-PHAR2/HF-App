#' hollow_fiber_1comp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_hollow_fiber_1comp_abs_ui <- function(id) {
  ns <- NS(id)
  tagList(fluidPage(
    tabsetPanel(
      id = "tabset1comp",
      type = "pills",
      tabPanel(
        "HF-Setup",
        fluidRow(
          mod_input_pump_ui(ns("input_pump_1comp_abs")),
          mod_input_exp_system_ui(ns("input_exp_system_1comp_abs")),
          mod_input_drug_charac_1comp_abs_ui(ns("input_drug_charac_1comp_abs")),
          mod_input_dosing_charac_1comp_abs_ui(ns("input_dosing_charac_1comp_abs"))
        ),
        fluidRow(
          column(
            12,
            align = "center",
            style = "padding-bottom:10px",
            mod_input_start_sim_button_ui(ns("input_start_sim_button_1comp_abs"))
          )
        ),
        fluidRow(
          mod_parameter_table_1comp_abs_ui(ns("parameter_table_1comp_abs")),
          mod_experiment_diagram_1comp_abs_ui(ns("experiment_diagram_1comp_abs"))
        ),
        fluidRow(
          mod_infusion_table_1comp_abs_ui(ns("infusion_table_1comp_abs")),
        )
      ),
      tabPanel(
        "Simulations plotting",
        mod_graph_concentrations_1comp_abs_ui(ns("graph_concentrations_1comp_abs"))
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
mod_hollow_fiber_1comp_abs_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    library(zeallot) #used for the %<-% operator
    c(minPumpFlow, maxPumpFlow, stepPumpFlow) %<-% mod_input_pump_server("input_pump_1comp_abs")
    c(Vcentral,
      debitCentralCartridge,
      Vcartridge,
      constantVolume) %<-% mod_input_exp_system_server("input_exp_system_1comp_abs")
    c(drugName,
      initialConcentration,
      lastTimePointHours,
      halfLifeHours,
      bioavailability,
      ka) %<-% mod_input_drug_charac_1comp_abs_server(
        "input_drug_charac_1comp_abs",
        minPumpFlow,
        maxPumpFlow,
        stepPumpFlow,
        Vcentral,
        Vcartridge
      )
    c(
      nInfusions,
      dosingIntervalHoursAbs,
      nDoses,
      CinfusionMin,
      CinfusionMax,
      CinfusionStep,
      minInfusionVolume
    ) %<-% mod_input_dosing_charac_1comp_abs_server("input_dosing_charac_1comp_abs")

    simulateButton <-
      mod_input_start_sim_button_server("input_start_sim_button_1comp_abs")

    parameterTable <-
      mod_parameter_table_1comp_abs_server(
        "parameter_table_1comp_abs",
        halfLifeHours,
        Vcentral,
        Vcartridge,
        initialConcentration,
        lastTimePointHours,
        drugName,
        debitCentralCartridge,
        nDoses,
        ka,
        f_avail = bioavailability,
        nInfusions,
        dosingIntervalHoursAbs,
        CinfusionMin,
        CinfusionMax,
        CinfusionStep,
        minInfusionVolume,
        simulateButton
      )

    infusionTable <-
      mod_infusion_table_1comp_abs_server(
        "infusion_table_1comp_abs",
        halfLifeHours,
        Vcentral,
        Vcartridge,
        initialConcentration,
        lastTimePointHours,
        drugName,
        debitCentralCartridge,
        nDoses,
        ka,
        f_avail = bioavailability,
        nInfusions,
        dosingIntervalHoursAbs,
        CinfusionMin,
        CinfusionMax,
        CinfusionStep,
        minInfusionVolume,
        simulateButton
      )

    imgFilePath <- resourcePaths()['img']

    experimentDiagram <-
      mod_experiment_diagram_1comp_abs_server(
        "experiment_diagram_1comp_abs",
        parameterTable,
        admType = "Absorb",
        drugName,
        imgFilePath,
        simulateButton
      )
    defaultDataFilePath <- resourcePaths()['default_data']
    modelFilePath <- resourcePaths()['models']
    simulatedDataReadyEventName = "simData1compAbsReady"
    simulatedData <-
      mod_simulate_concentrations_1comp_abs_server(
        "simulate_concentrations_1comp_abs",
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
        f_avail = bioavailability,
        nInfusions,
        dosingIntervalHoursAbs,
        CinfusionMin,
        CinfusionMax,
        CinfusionStep,
        minInfusionVolume,
        simulateButton
      )


    mod_graph_concentrations_1comp_abs_server(
      "graph_concentrations_1comp_abs",
      simulatedData,
      lastTimePointHours,
      dosingIntervalHoursBolus = NA_real_,
      dosingIntervalHoursAbs,
      admType = "Infusion",
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
