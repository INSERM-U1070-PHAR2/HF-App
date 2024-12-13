#' graph_concentrations_1comp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_graph_concentrations_1comp_ui <- function(id) {
  ns <- NS(id)
  tagList(
    plotly::plotlyOutput(ns("HFSim1Comp"), height =
                           "800px"),
    fileInput(
      ns("dataFile"),
      "Add data to graph (.csv)",
      multiple = FALSE,
      accept = c("csv")
    ),
    varSelectInput(ns("x"),
                   "x variable:",
                   data = NULL),
    varSelectInput(ns("y"),
                   "y variable:",
                   data = NULL)
  )
}

#' graph_concentrations_1comp Server Functions
#'
#' @noRd
mod_graph_concentrations_1comp_server <- function(id,
                                                   simulatedData,
                                                   lastTimePointHours,
                                                   dosingIntervalHoursBolus,
                                                   dosingIntervalHoursInf,
                                                   admType,
                                                   drugName,
                                                   simulateButton) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    library(magrittr)
    # valuesForPlotting <- reactiveValues(observedData = NULL)

    uploadedData <- reactive({
      file <- input$dataFile
      if (is.null(file))
        return(NULL)
      else
        return(read.csv(file$datapath))
    })

    observeEvent(input$dataFile,
                 ({
                   updateVarSelectInput(inputId = "y",
                                        data = uploadedData())
                   updateVarSelectInput(inputId = "x",
                                        data = uploadedData())
                 }))

    base_plot <- reactive({
      req(simulatedData())
      HollowFibre1CompPlot(
        simulatedData(),
        lastTimePointHours(),
        dosingIntervalHoursBolus(),
        dosingIntervalHoursInf(),
        admType(),
        drugName()
      )
    })



    PK1CompSimuPlot <- reactive({
      df <- uploadedData()

      if (is.null(df))
        return(base_plot())
      else
        return({
          base_plot() %>%
            plotly::add_trace(
              data = df,
              type = 'scatter',
              mode = 'markers',
              x = ~ get(input$x),
              y = ~ get(input$y),
              name = drugName(),
              hovertemplate = paste(
                '<b>Concentration</b>: %{y:.2f} mg/L',
                '<br><b>Time</b>: %{x:.2f} h'
              ),
              color = I("red")
            )
        })
    })

    output$HFSim1Comp <- plotly::renderPlotly({
      PK1CompSimuPlot() %>%
        plotly::config(toImageButtonOptions = list(format = 'svg'))
    })


  })
}

## To be copied in the UI
# mod_graph_concentrations_1comp_ui("graph_concentrations_1comp_1")

## To be copied in the server
# mod_graph_concentrations_1comp_server("graph_concentrations_1comp_1")
