#' experiment_diagram_2comp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_experiment_diagram_2comp_ui <- function(id){
  ns <- NS(id)
  tagList(
    box(
      title = "Diagram",
      status = "primary",
      width = 8,
      solidHeader = TRUE,
      collapsible = TRUE,
      plotly::plotlyOutput(ns("HFDiagram2Comp"), height =
                             "800px")
    )
  )
}

#' experiment_diagram_2comp Server Functions
#'
#' @noRd
mod_experiment_diagram_2comp_server <- function(id,
                                                parameterTable,
                                                admType,
                                                imgFilePath,
                                                simulateButton){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    observeEvent(simulateButton(), ({
      HFDiagram <- HollowFibre2CompDiagram(parameterTable(),
                                           admType(),
                                           imgFilePath)


      output$HFDiagram2Comp <- plotly::renderPlotly({
        image_file <- file.path(imgFilePath,"2CompApp.png")
        txt <-
          RCurl::base64Encode(readBin(image_file, "raw", file.info(image_file)[1, "size"]), "txt")
        return(
          plotly::ggplotly(HFDiagram, height = 800) %>%
            plotly::layout(images = list(
              list(
                source = paste('data:image/png;base64', txt, sep = ','),
                xref = "x",
                yref = "y",
                x = 0,
                y = 0,
                sizex = 2655,
                sizey = 1972,
                opacity = 1,
                layer = "below",
                sizing = "stretch"
              )
            ),
            hovermode = FALSE) %>%
            plotly::config(toImageButtonOptions = list(format = 'svg'))
        )
      })

    }))

    return(HFDiagram = reactive({
      HFDiagram()
    }))
  })
}

## To be copied in the UI
# mod_experiment_diagram_2comp_ui("experiment_diagram_2comp_1")

## To be copied in the server
# mod_experiment_diagram_2comp_server("experiment_diagram_2comp_1")
