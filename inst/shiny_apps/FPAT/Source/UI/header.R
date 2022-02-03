Header_UI <- function(id, label="header") {
  ns <- NS(id)
  tagList(
  column(12,
           column(1,h1("FPAT"),style="height:65px"),
           column(5,h3("Fisheries Performance Assessment Toolkit"), style="height:65px;padding-top:8px")

           # column(6,style="padding-top:25px",
           #        div(style="display: inline-block;vertical-align:top;float:right;",
           #            dropdownButton(inputId="FPAT_File",
           #                           column(12,
           #                                  h5(tags$b("Glossary",style="color:#347ab6")),
           #                                  column(12,
           #                                         tabsetPanel(
           #                                           tabPanel(h5("FPI ",style = "color:black"), HTML("<br>"), DT::dataTableOutput('CMPhelp'),value=1),
           #                                           tabPanel(h5("openMSE",style = "color:black"), HTML("<br>"), DT::dataTableOutput('PMhelp'),value=2),
           #                                           tabPanel(h5("FPAT",style = "color:black"), HTML("<br>"), value=3)
           #                                         )# end of dropdownbutton CMP
           #                                  ),
           #                                  column(12,HTML("<br>")),
           #                                  h5(tags$b("Contact",style="color:#347ab6")),
           #                                  column(12,
           #                                         h5("For technical questions or bug reports please contact ", a("tom@bluematterscience.com", href="mailto:tom@bluematterscience.com", target="_blank"),style = "color:grey")
           #                                  ),
           #                                  h5(tags$b("Software",style="color:#347ab6")),
           #                                  column(12,
           #                                         h5("FPAT v0.1.0",style = "color:grey")
           #                                  ),
           #                                  h5(tags$b("Acknowledgements",style="color:#347ab6")),
           #                                  column(12,
           #                                         h5("FAO, UW, openMSE sponsors")
           #                                  )
           #                           ),
           #                           label = "Help",
           #                           icon = icon("info"),
           #                           status = "dropdownbutton",
           #                           right=TRUE,
           #                           circle = FALSE,
           #                           width="800px"
           #            )
           #        ), # end of help menu dropdown
           #
           #        # Reports menu dropdown
           #        div(style="display: inline-block;vertical-align:top; float:right;",
           #            dropdownButton(
           #              column(12,
           #                     column(9,h5("Operating model",style="font-weight:bold;color:#347ab6")),
           #                     column(3,downloadButton("OM_Rep"," ")),
           #                     column(9,h5("Conditioning",style="font-weight:bold;color:#347ab6")),
           #                     column(3,downloadButton("Cond_Rep"," ")),
           #                     column(9,h5("FPAT results",style="font-weight:bold;color:#347ab6")),
           #                     column(3,downloadButton("FPAT_Rep"," "))
           #              ),
           #              inputId = "Reports",
           #              label = "Reports",
           #              icon = icon("newspaper"),
           #              status = "dropdownbutton",
           #              circle = FALSE,
           #              right=TRUE,
           #              width="300px"
           #            )
           #        ), # end of reports menu dropdown
           #
           #        # Settings menu dropdown
           #        div(style="display: inline-block;vertical-align:top; float:right;",
           #            dropdownButton(
           #              column(12,
           #                     h5(tags$b("Settings for controlling OM conditioning etc",style="color:#347ab6")),
           #              ),
           #              inputId = "DD_Settings",
           #              label = "Settings",
           #              icon = icon("cogs"),
           #              status = "dropdownbutton",
           #              circle = FALSE,
           #              right=TRUE,
           #              width="400px"
           #            )
           #
           #        ), # end of settings menu dropdown
           #
           #        # File menu dropdown
           #        div(style="display: inline-block;vertical-align:top; float:right;",
           #            dropdownButton(
           #              column(12,tags$hr(style="margin-top: 3px; margin-bottom: 3px"),
           #                     h5(tags$b("FPAT Session",style="color:#347ab6")),
           #                     column(6,h5("Load (.fpat)",style = "color:grey"),
           #                            tipify(fileInput("Load_session",label=NULL,accept=c("fpat",".fpat")),
           #                                   title="Load a previous session including calculated results (large)")),
           #                     column(1),
           #                     column(5,h5("Save (.fpat)",style = "color:grey"),    downloadButton("Save_session","",width="100px"))
           #              ),
           #              inputId = "DD_file",
           #              label = "File",
           #              icon = icon("file"),
           #              status = "dropdownbutton",
           #              circle = FALSE,
           #              right=TRUE,
           #              width="400px"
           #            )
           #        ) # end of file menu dropdown
           # )  # end of tool bar
    )
  )
}
