
fluidPage(style="position: absolute;  padding-left: 40px; padding-right: 40px;",
          useShinyalert(),
          useShinydashboard(),
          includeScript(path = "www/js/js4checkbox.js"),
          includeScript(path = "www/js/index.js"),

          tags$head(
            tags$link(rel='stylesheet', type='text/css', href='styles.css'),
            tags$link(href="fa/css/all.css", rel="stylesheet"), # font-awesome
            tags$style(HTML("#SessionID{font-size:12px;}")),
            tags$script('
        var dimension = [0, 0];
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
    ')
          ),

          # === HEADER =================================================================
          Header_UI('header'),

          # === MAIN WINDOW ============================================================
          column(12, # General tab panel
                 div(class='left_menu',
                     verticalTabsetPanel(id = "NonTech",selected=1, color='#4291be',

                                         verticalTabPanel(value=1,
                                                          h5(strong("Home")),
                                                          height="400px",
                                                          Home_UI('Home1'),
                                                          box_height='55px'),

                                         verticalTabPanel(value=2,
                                                          h5(strong("1. Load")),
                                                          Load_UI('Load1'),
                                                          box_height='55px'),

                                         verticalTabPanel(value=3,
                                                          h5(strong("2. FPI Scores")),
                                                          FPI_UI('FPI1'),
                                                          box_height='55px'),

                                         verticalTabPanel(value=4,
                                                          h5(strong("3. Fishery Dynamics")),
                                                          HistDynamics_UI('dynamics'),
                                                          box_height='55px'),

                                         verticalTabPanel(value=5,
                                                          h5(strong("4. Projections")),
                                                          Results_UI('Results1'),
                                                          box_height='55px'),
                                         contentWidth=11) # end of tabsetpanel
                 )

          ), # end of main window for general


          # === LOG ====================================================================

          column(12,
                 br(),
                 verbatimTextOutput("Log",placeholder=T),
                 bsTooltip("Log","Application Log"),
                 downloadButton("Download_Log","Download Log",style="height:28px"),
                 hr()
          ),


          # === SESSION INFO & COPYRIGHT ===============================================
          column(12, style="height:40px;  text-align: center;",
                 br(),
                 textOutput("SessionID"),
                 h6(
                   "Copyright", HTML("&#169;"),
                   a(paste("Blue Matter Science Ltd,", format(Sys.Date(), "%Y")),
                     href="https://www.bluematterscience.com/", target="_blank")
                 ),
                 htmlOutput('package_versions'),
                 br()
          )

) # end of fluid page

