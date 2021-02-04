

fluidPage(

  includeScript(path = "www/js/js4checkbox.js"),
  includeScript(path = "www/js/index.js"),

  tags$head(
    tags$link(rel='stylesheet', type='text/css', href='styles.css'),

    tags$link(href="fa/css/all.css", rel="stylesheet"), # font-awesome
    tags$style(HTML("
                    #SessionID{font-size:12px;}
                    ")),
    tags$style(HTML("
        /* https://fonts.google.com/?preview.text=SLICK&preview.text_type=custom */

        @import url('//fonts.googleapis.com/css?family=Cairo|Cabin:400,700');

        /* Font of SLICK title */

      ")),
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

  # === HEADER ==============================================================================================================================================================
  column(7,
         column(2,h1("FPAT"),style="height:65px"),
         column(10,h3("fisheries performance assessment toolkit"),style="height:65px;padding-top:8px")
  ),

  # === MAIN WINDOW =========================================================================================================================================================

  column(12, # General tab panel
    verticalTabsetPanel(id = "NonTech",selected=1,

      verticalTabPanel(value=1,
                       h5(strong("Home")),
                       Home_UI('Home1'),
                       box_height='50px'),

      verticalTabPanel(value=2,
                       h5("Fishery"),
                       Fishery_UI('Fishery1'),
                       box_height='50px'),

      verticalTabPanel(value=3,
                       h5("Management"),
                       Manage_UI('Manage1'),
                       box_height='50px'),

      verticalTabPanel(value=4,
                       h5(strong("Help")),
                       Help_UI('Help1'),
                       box_height='50px'),

    contentWidth=11

    ) # end of tabsetpanel

  ), # end of main window for general


  column(12,  br(),br(), style="height:40px;  text-align: center;",textOutput("SessionID")),

  column(12,  br(),style="height:40px; text-align: center", h6("copyright (c) Blue Matter Science Ltd, 2021"))
        #h5("Bottom of app (Version etc)"),
         #verbatimTextOutput("Log2",placeholder=T)

         # verbatimTextOutput("Temp",placeholder=T)
         #)

) # end of fluid page
