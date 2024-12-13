#' parameter_table_2comp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_parameter_table_2comp_ui <- function(id){
  ns <- NS(id)
  tagList(
    box(
      title = "Parameters",
      status = "primary",
      solidHeader = TRUE,
      collapsible = TRUE,
      width = 4,
      div(style = 'overflow-x: scroll', uiOutput(ns(
        "HFParameters2Comp"
      ))),
      downloadButton(ns('downloadTable'), 'Download table')
    )
  )
}

#' parameter_table_2comp Server Functions
#'
#' @noRd
mod_parameter_table_2comp_server <- function(id,
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
                                             simulateButton){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    values <- reactiveValues()

    observeEvent(simulateButton(), ({

      values$parameterTable <- HollowFibre2CompParam(
        as.numeric(halfLifeHoursA()),
        as.numeric(halfLifeHoursB()),
        Vcentral(),
        Vcartridge(),
        drugNameA(),
        drugNameB(),
        initialConcentrationA(),
        initialConcentrationB(),
        lastTimePointHours(),
        admType(),
        VinjectBolusA(),
        dosingIntervalHoursBolusA(),
        numberOfDosesBolusA(),
        VinjectBolusB(),
        VinjectBolusExtraB(),
        dosingIntervalHoursBolusB(),
        numberOfDosesBolusB(),
        tinfuseHoursA(),
        VinjectInfA(),
        dosingIntervalHoursInfA(),
        numberOfDosesInfA(),
        tinfuseHoursB(),
        VinjectInfB(),
        VinjectInfExtraB(),
        dosingIntervalHoursInfB(),
        numberOfDosesInfB(),
        debitCentralCartridge()
      )

      flexTable <-
        values$parameterTable[, c("Group", "Parameter", "Value")] %>%
        flextable::as_grouped_data(x = ., groups = c("Group")) %>%
        flextable::as_flextable() %>%
        flextable::compose(
          i = ~ !is.na(Group),
          # when Group not NA
          j = "Parameter",
          # on column "parameter"
          # create a paragraph containing a chunk containing value of `var_group`
          value = flextable::as_paragraph(flextable::as_chunk(Group))
        ) %>%
        flextable::bold(
          j = 1,
          i = ~ !is.na(Group),
          bold = TRUE,
          part = "body"
        ) %>%
        flextable::bold(part = "header", bold = TRUE) %>%
        flextable::autofit()

      output$HFParameters2Comp <- renderUI({
        flexTable %>%
          flextable::htmltools_value()
      })

      output$downloadTable <- downloadHandler(
        filename = function() {
          "example.docx"
        },
        content = function(file) {
          ft <- flexTable
          doc <- officer::read_docx()
          doc <- flextable::body_add_flextable(doc, value = ft)
          print(doc, target = file)
        }
      )
    }))

    return(parameterTable = reactive({
      values$parameterTable
    }))

  })
}

## To be copied in the UI
# mod_parameter_table_2comp_ui("parameter_table_2comp_1")

## To be copied in the server
# mod_parameter_table_2comp_server("parameter_table_2comp_1")
