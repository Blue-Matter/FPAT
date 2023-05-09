HelpUI <- function(id, label="settings") {

  ns <- NS(id)
  tagList(
    fluidRow(
      p('help')

    )
  )
}

SettingsUI <- function(id, label="settings") {

  ns <- NS(id)
  tagList(
    fluidRow(
      p('Settings')

    )
  )
}




# -- header ----
header <- shinydashboardPlus::dashboardHeader(
  leftUi = tagList(
    dropdownButton(
      label = "Help",
      icon = icon("info"),
      status = "primary",
      circle = FALSE,
      uiOutput("language")
    )),
  controlbarIcon=shiny::icon('gears')
)

# -- rhs controlbar ----
controlbar <- dashboardControlbar(
  skin = "dark",
  controlbarMenu(
    id = "menu",
    controlbarItem(
      "Tab 1",
      "Welcome to tab 1"
    ),
    controlbarItem(
      "Tab 2",
      "Welcome to tab 2"
    )
  )
)



# -- lhs sidebar ----
sidebar <- dashboardSidebar(
  collapsed = TRUE,
  sidebarMenu(id='NonTech',
              menuItem("Home", tabName = "home", icon = icon("house")),
              menuItem("1. Load", tabName = "load", icon = icon("upload")),
              menuItem("2. FPI Scores", tabName = "fpi", icon = icon("chart-bar")),
              menuItem("3. Fishery Dynamics", tabName = "dynamics", icon = icon("chart-scatter")),
              menuItem("4. Projections", tabName = "projections", icon = icon("chart-line"))
  )
)


# -- body ----
body <- dashboardBody(
  tags$head(
    includeScript(path = "www/js/js4checkbox.js"),
    includeScript(path = "www/js/index.js"),
    tags$link(rel='stylesheet', type='text/css', href='styles.css'),
    tags$link(href="fa/css/all.css", rel="stylesheet"), # font-awesome
    tags$link(rel="shortcut icon", href="favicon.ico"),

    tags$style(HTML("#SessionID{font-size:12px;}")),
    tags$style(HTML("/* https://fonts.google.com/?preview.text=SLICK&preview.text_type=custom */
        @import url('//fonts.googleapis.com/css?family=Cairo|Cabin:400,700');
        /* Font of SLICK title */
      ")),
    tags$script(
      'var dimension = [0, 0];
    $(document).on("shiny:connected", function(e) {
      dimension[0] = window.innerWidth;
      dimension[1] = window.innerHeight;
      Shiny.onInputChange("dimension", dimension);
    });
    $(window).resize(function(e) {
      dimension[0] = window.innerWidth;
      dimension[1] = window.innerHeight;
      Shiny.onInputChange("dimension", dimension);
    });
    '),
    tags$script("
        var openTab = function(tabName){
          $('a', $('.sidebar')).each(function() {
            if(this.getAttribute('data-value') == tabName) {
              this.click()
            };
          });
        }
      ")

  ),
  tabItems(
    tabItem(tabName = "home",
            Home_UI('home')
    ),
    tabItem(tabName = "load",
            Load_UI('load')
    ),
    tabItem(tabName = "fpi",
            FPI_UI('FPI')
    ),
    tabItem(tabName = "dynamics",
            Dynamics_UI('dynamics')
    ),
    tabItem(tabName = "projections",
            Results_UI('results')
    )
  )
)


# -- page ----

dashboardPage(
  skin = "blue-light",
  header=header,
  sidebar=sidebar,
  body=body,
  controlbar=controlbar,
  title='FPAT',
  dashboardFooter(left =  div(htmlOutput('package_versions'), textOutput("SessionID")),
                  right = h6("Copyright", HTML("&#169;"), tags$a(href='https://bluematterscience.com/',
                                 target="_blank", paste0("Blue Matter Science Ltd.", format(Sys.Date(), "%Y")))))
)
