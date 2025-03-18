#' parameter_table_1_comp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_parameter_table_1comp_abs_ui <- function(id) {
  ns <- NS(id)
  tagList(
    box(
      title = "Parameters",
      status = "primary",
      solidHeader = TRUE,
      collapsible = TRUE,
      width = 4,
      div(style = 'overflow-x: scroll', uiOutput(ns(
        "HFParameters1Comp"
      ))),
      downloadButton(ns('downloadTable'), 'Download table')
    )
  )
}

#' parameter_table_1_comp Server Functions
#'
#' @noRd
mod_parameter_table_1comp_abs_server <- function(id,
                                                 halfLifeHours,
                                                 Vcentral,
                                                 Vcartridge,
                                                 initialConcentration,
                                                 lastTimePointHours,
                                                 drugName,
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
                                             simulateButton) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    values <- reactiveValues()

    observeEvent(simulateButton(), ({

      values$parameterTable <- HollowFibre1CompAbsParam(
        halfLifeHours = as.numeric(halfLifeHours()),
        Vcentral = Vcentral(),
        Vcartridge = Vcartridge(),
        initial_concentration = initialConcentration(),
        lastTimePointHours = lastTimePointHours(),
        drugName = drugName(),
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

      output$HFParameters1Comp <- renderUI({
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
    }), ignoreNULL = FALSE)

    return(parameterTable = reactive({
      values$parameterTable
    }))
  })
}

## To be copied in the UI
# mod_parameter_table_1comp_ui("parameter_table_1comp")

## To be copied in the server
# mod_parameter_table_1comp_server("parameter_table_1comp")
