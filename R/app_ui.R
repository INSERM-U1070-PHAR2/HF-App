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
    # 1st page = standard hollow fiber 1 drug
    shinydashboard::dashboardPage(
      title = "HF-App",
      shinydashboard::dashboardHeader(title = "HF-App",
                                      tags$li(
                                        a(
                                          onclick = "onclick =window.open('https://github.com/Vincent-AC/HF-App')",
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
          shinydashboard::menuItem("1 compartment Hollow Fibre",
                                   tabName = "1comp"),
          shinydashboard::menuItem("2 compartment Hollow Fibre",
                                   tabName = "2comp")
        )
      ),
      shinydashboard::dashboardBody(shinydashboard::tabItems(
        shinydashboard::tabItem(tabName = "home",
                                mod_welcome_page_ui("welcome_page")),
        shinydashboard::tabItem(tabName = "1comp",
                                mod_hollow_fiber_1comp_ui("hf_1comp")),
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
  tags$head(favicon(),
            bundle_resources(path = app_sys("app/www"),
                             app_title = "HFApp"))
            # Add here other external resources
            # for example, you can add shinyalert::useShinyalert())
}
