#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    shinybusy::add_busy_spinner(spin = "fading-circle",
                                position = "full-page"),
    tags$style(".shinybusy-overlay {opacity: 0.7; background-color: #7c7c7c;content=\"loading\";}"), #to add some transparency
    # 1st page = standard hollow fiber 1 drug
    shinydashboard::dashboardPage(
      title = "HF-App",
      shinydashboard::dashboardHeader(title = "HF-App",
                                      tags$li(
                                        a(
                                          onclick = "onclick =window.open('https://github.com/INSERM-U1070-PHAR2/HF-App')",
                                          href = NULL,
                                          icon("github"),
                                          title = "GitHub",
                                          style = "cursor: pointer;"
                                        ),
                                        class = "dropdown"
                                      )),
      shinydashboard::dashboardSidebar(
        shinydashboard::sidebarMenu(
          shinydashboard::menuItem("Start here !",
                                   tabName = "home"),
          shinydashboard::menuItem("Quick-start guide (new tab)",
                                   href = "www/instructions_html.html",
                                   newtab = TRUE),
          shinydashboard::menuItem("1 compartment Hollow Fibre",
                                   shinydashboard::menuSubItem("Intravenous",tabName = "1comp"),
                                   shinydashboard::menuSubItem("First order absorption",tabName = "1compAbs"),
                                   startExpanded = TRUE
          ),
          shinydashboard::menuItem("2 compartment Hollow Fibre",
                                   tabName = "2comp")
        )
      ),
      shinydashboard::dashboardBody(shinydashboard::tabItems(
        shinydashboard::tabItem(tabName = "home",
                                mod_welcome_page_ui("welcome_page")),
        shinydashboard::tabItem(tabName = "1comp",
                                mod_hollow_fiber_1comp_ui("hf_1comp")),
        shinydashboard::tabItem(tabName = "1compAbs",
                                mod_hollow_fiber_1comp_abs_ui("hf_1comp_abs")),
        shinydashboard::tabItem(tabName = "2comp",
                                mod_hollow_fiber_2comp_ui("hf_2comp"))
      ))
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path("www",
                    app_sys("app/www"), )
  add_resource_path("img",
                    app_sys("app/img"), )
  add_resource_path("models",
                    app_sys("app/models"), )
  add_resource_path("default_data",
                    app_sys("app/default_data"), )
  tags$head(favicon(),
            bundle_resources(path = app_sys("app/www"),
                             app_title = "HFApp"))
            # Add here other external resources
            # for example, you can add shinyalert::useShinyalert())
}
