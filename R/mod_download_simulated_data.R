#' download_simulated_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_download_simulated_data_ui <- function(id) {
  ns <- NS(id)
  tagList(
    box(
      title = "Download data",
      status = "primary",
      width = 12,
      solidHeader = TRUE,
      collapsible = TRUE,
      tableOutput(ns('firstLinesOfData')),
      downloadButton(ns('downloadData'), 'Download simulated data')
    )
  )
}

#' download_simulated_data Server Functions
#'
#' @noRd
mod_download_simulated_data_server <- function(id,
                                               simulatedData) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$firstLinesOfData <- renderTable({
      data <- simulatedData()
      head(data)
    })
    output$downloadData <- downloadHandler(
      filename = function() {
        "simulated_data.csv"
      },
      content = function(file) {
        data <- simulatedData()
        readr::write_csv(data, file)
      }
    )

  })
}

## To be copied in the UI
# mod_download_simulated_data_ui("download_simulated_data_1")

## To be copied in the server
# mod_download_simulated_data_server("download_simulated_data_1")
