#' graph_concentrations_2comp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_graph_concentrations_2comp_ui <- function(id){
  ns <- NS(id)
  tagList(
    plotly::plotlyOutput(ns("HFSim2Comp"), height =
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
                   data = NULL),
    varSelectInput(ns("drugColumn"),
                   "Drug variable:",
                   data = NULL),
    selectInput(ns("drugAValueGraph"),
                "Drug A value:",
                choices = ""),
    selectInput(ns("drugBValueGraph"),
                "Drug B value:",
                choices = "")
  )
}

#' graph_concentrations_2comp Server Functions
#'
#' @noRd
mod_graph_concentrations_2comp_server <- function(id,
                                                  simulatedData,
                                                  lastTimePointHours,
                                                  drugNameA,
                                                  drugNameB,
                                                  admType){
  moduleServer( id, function(input, output, session){
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

    observeEvent(uploadedData(),
                 ({
                   updateVarSelectInput(inputId = "y",
                                        data = uploadedData())
                   updateVarSelectInput(inputId = "x",
                                        data = uploadedData())
                   updateVarSelectInput(inputId = "drugColumn",
                                        data = uploadedData())
                 }))
    observeEvent(input$drugColumn,
                 ({
                   items <- unique(uploadedData()[[input$drugColumn]])
                   updateSelectInput(inputId = "drugAValueGraph",
                                        choices = items)
                   updateSelectInput(inputId = "drugBValueGraph",
                                        choices = items)
                 }))

    base_plot <- reactive({
      req(simulatedData())
      HollowFibre2CompPlot(
        simulatedData(),
        lastTimePointHours(),
        drugNameA(),
        drugNameB(),
        admType()
      )
    })



    PK2CompSimuPlot <- reactive({
      df <- uploadedData()

      if (is.null(df))
        return(base_plot())
      else
        return({
          req(input$drugAValueGraph,input$drugBValueGraph)
          dfA <-
            filter(df,!!sym(input$drugColumn) == input$drugAValueGraph) %>%
            type.convert()
          dfB <-
            filter(df,!!sym(input$drugColumn) == input$drugBValueGraph) %>%
            type.convert()

          base_plot() %>%
            plotly::add_trace(
              data = dfA,
              type = 'scatter',
              mode = 'markers',
              x = ~ get(input$x),
              y = ~ get(input$y),
              name = drugNameA(),
              hovertemplate = paste(
                '<b>Concentration</b>: %{y:.2f} mg/L',
                '<br><b>Time</b>: %{x:.2f} h'
              ),
              color = I("red")
            ) %>%
            add_trace(
              data = dfB,
              type = 'scatter',
              mode = 'markers',
              x = ~ get(input$x),
              y = ~ get(input$y),
              name = drugNameB(),
              hovertemplate = paste(
                '<b>Concentration</b>: %{y:.2f} mg/L',
                '<br><b>Time</b>: %{x:.2f} h'
              ),
              color = I(c("blue"))
            )
        })
    })

    output$HFSim2Comp <- plotly::renderPlotly({
      PK2CompSimuPlot() %>%
        plotly::config(toImageButtonOptions = list(format = 'svg'))
    })
  })
}

## To be copied in the UI
# mod_graph_concentrations_2comp_ui("graph_concentrations_2comp_1")

## To be copied in the server
# mod_graph_concentrations_2comp_server("graph_concentrations_2comp_1")
