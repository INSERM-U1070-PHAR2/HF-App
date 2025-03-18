#' welcome_page UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_welcome_page_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(fluidRow(
      box(
        title = "Welcome",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        width = 6,
        tags$p(align = "center",
               "Welcome to HF-App !",
               br(),    br(),
               "The R-Shiny application made to streamline hollow-fibre
    experimental setup.",
               br(),    br(),
               "The app currently supports hollow-fibre with one or two
    drugs with differing half-lives."),
        br(),    br(),
        "It has been presented at ECCMID 2021 as an online poster which can be found ",
        tags$a(href = "https://eacademy.escmid.org/escmid/2021/eccmid-2021/328390/vincent.aranzana-climent.hf-app.a.r-shiny.web.application.to.streamline.html?f=listing%3D0%2Abrowseby%3D8%2Asortby%3D1%2Asearch%3Daranzana+climent",
               target = "_blank",
               title = "poster",
               "here"),
        br(),    br(),
        "For any suggestions, feed-back, bug reports, please contact me through
    email or any other mean by clicking on the relevent icon in the
    'Contributors' box (on the right)",
        br(),    br(),
        "To cite this application :   Vincent Aranzana-Climent, Alexia Chauzy and Nicolas Grégoire (2021).
    HF-App: A R-Shiny application to streamline hollow-fibre experiments. R application version
  1.0.0. https://varacli.shinyapps.io/hollow_fiber_app/",
        br(),    br(),
        "This software is free and open-source, distributed under the French
        government open licence version 2. See the full license on github"
      ),
      box(
        title = "Contributors",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        width = 6,
        tags$p(
          "Main developper and maintainer :",
          br(),    br(),
          "Vincent Aranzana-Climent",
          tags$a(
            href = "https://github.com/Vincent-AC/",
            title = "github",
            target = "_blank",
            rel = "noopener noreferrer",
            icon("github")
          ),
          tags$a(
            href = "mailto:vincent.aranzana.climent@univ-poitiers.fr",
            title = "email",
            target = "_blank",
            rel = "noopener noreferrer",
            icon("envelope")
          ),
          tags$a(
            href = "https://orcid.org/0000-0002-1258-8054",
            title = "orcid",
            target = "_blank",
            rel = "noopener noreferrer",
            icon("orcid")
          ),
          tags$a(
            href = "https://www.linkedin.com/in/vincent-aranzana-climent-698a6884/",
            title = "linkedin",
            target = "_blank",
            rel = "noopener noreferrer",
            icon("linkedin")
          ),
          br(),    br(),
          "Testers and advisors:",
          br(),    br(),
          "Alexia Chauzy, Nicolas Grégoire, Noémie Prébonnaud",
          br(),    br(),
          "Special thanks to Karin Allander for sharing her hollow-fibre diagram",
          br(),    br(),
          "Special thanks to ISAP and EPASG for promoting our tool to their communities"
        )
      )
    ),
    fluidRow(
      box(    title = "Quick-start guide",
              status = "primary",
              solidHeader = TRUE,
              collapsible = TRUE,
              width = 12,
              tags$a(
                href = "www/instructions_html.html",
                title = "popout_instructions",
                onclick = "window.open('www/instructions_html.html','newwindow','width=300,height=250');return false;",
                rel = "noopener noreferrer",
                "Show instructions in a new window"
              ),
              br(),
              "Here is a quick rundown of how to setup a 1 compartment hollow-fibre with this application.",
              br(),    br(),
              "1. In the top left menu, click on 1 compartment hollow-fibre",
              br(),    br(),
              "2. In the HF-Setup tab, the first box is called Pump characteristics.
          Here you inform the app of what pump flows your setup is able to produce.
          This, in addition to the volumes in the bottles will condition which
          half-lives it is possible to simulate",
              br(),    br(),
              "3. In the Experimental system characteristics box, you inform which
          volumes of media you will have in the central compartment bottle,
          in the hollow-fibre cartridge, and also to which value you set the
          media flow between the central bottle and the cartridge. At the bottom,
          the tickbox 'is volume constant?' represents assumption on whether the
          volume of media that you inject when you add the drug is negligible
          when compared with the total volume in the setup. If you untick this
          box increase of volume in the central bottle due to drug injection(s)
          will be taken into consideration in the PK simulations.",
              br(),    br(),
              "4. In the Drug Characteristics box, you first give the name of the drug,
          (only used for graph legends). Then you can select an half-life to be
          simulated in a drop-down menu. The values in these lists are all the
          possible half-lives able to be simulated given your pump specifications
          and the volumes in the central bottle + cartridge. If the desired
          half-life is not in the list, adjusting the volumes in the system or
          using pumps with different characteristics will change the range of
          possible half-lives. Also, you need to provide the desired peak
          concentration after first dose and finally the duration of the
          experiment, which is used for graphs and calculation of the total
          amount of media needed and expanded.",
              br(),    br(),
              "5. In the Dosing Characteristics box, you first select the type of
          administration, then provide info about this administration. Note that
          for the loading dose + infusion, the infusion is continuous.",
              br(),    br(),
              "6. After setting all these parameters and after every time that you
          change any parameter, click on the 'Simulate HF' button. It will
          regenerate all outputs after a few seconds.",
              br(),    br(),
              "Outputs : ",
              br(),    br(),
              "1. At the bottom right of the table you can find a table
          recapitulating all experimental parameters plus some added calculated
          outputs like the volume de diluant spent and pump flows. This table is
          downloadble via the button at the bottom left in a Microsoft Word
          format",
              br(),    br(),
              "2. At the bottom right, you have a diagram that graphically recapitulates
          the setup. It is interactive so you can zoom in and out. It is
          downloadable in .svg format by clicking on the camera icon at the top
          of the diagram.",
              br(),    br(),
              "3. On the next tab, the 'Simulation plotting' tab, you first find an
          interactable graph showing the expected drug concentrations in the
          central bottle (dashed line), and in the cartridge (full line). In
          normal conditions these two overlap, but if the flow from central to
          cartridge is low enough you can distinguish them. By pointing your
          cursor on any point of the lines, you will see the concentrations at
          that point appear on screen. The graph is downloadable in an .svg
          format by clicking on the camera icon at the top of the graph.",
              br(),    br(),
              "4. After the experiment is done, you can come back to this graph and
          add the measured concentrations as dots on it. To do so you need to
          upload your data in .csv format. This is done at the bottom of the graph.
          The data needs to be formatted so that one row = one measurement,
          which means that for the one drug case you need to have a column with
          the time of measurement and one column for the measured concentration.
          After uploading the csv file you will have to tell the app which
          column label corresponds to the time (x variable) and which
          corresponds to the conentration (y variable). After adding the points
          to the graph, you can download the modified graph by clicking on the
          camera icon at the top of the graph.",
              br(),    br(),
              "5. By clicking on the third tab 'Simulated data' you can download the
          simulated data that was used for the simulation graph in .csv format
          for further use."

      )
    )
    )
  )
}

#' welcome_page Server Functions
#'
#' @noRd
mod_welcome_page_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_welcome_page_ui("welcome_page_1")

## To be copied in the server
# mod_welcome_page_server("welcome_page_1")
