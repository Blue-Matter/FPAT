TradeOffUI <- function(id) {

  ns <- NS(id)
  tagList(
    uiOutput(ns('tradeoff'))
  )
}

TradeOffServer <- function(id, Info, input, window_dims, ...) {

  moduleServer(id, function(input, output, session) {

    output$TradeOff_plot <- renderPlot({
      MSEproj2 <- Sub(Info$MSEproj, MPs=input$TO2_MP_select)

      Xaxis <- list()
      Xaxis$Year <- input$x_year
      Xaxis$Var <- input$x_var
      Xaxis$Reference <- input$x_reference
      Xaxis$Metric <- input$x_metric
      IncEx <- input$x_err
      if (length(Xaxis$Reference)<1) Xaxis$Reference <- 0
      if(length(IncEx)<1) IncEx <- FALSE

      Yaxis <- list()
      Yaxis$Year <- input$y_year
      Yaxis$Var <- input$y_var
      Yaxis$Reference <- input$y_reference
      Yaxis$Metric <- input$y_metric
      IncEy <- input$y_err
      if (length(Yaxis$Reference)<1) Yaxis$Reference <- 0
      if(length(IncEy)<1) IncEy <- FALSE

      TradeOff_plot(MSEproj2, Xaxis, Yaxis, IncEx, IncEy)
    },
    width=function() {
      dims <- window_dims()
      dims[1]*0.3
    },
    height=function() {
      dims <- window_dims()
      dims[1]*0.3
    })

    TO_height <- reactiveVal(300)

    observe({
      if(!is.null(Info$MSEproj)) {
        dims <- window_dims()
        plotheight <- dims[1]*0.3
        TO_height(plotheight)
      }
    })

    output$TradeOff_plot_group <- renderPlot({
      MSEproj2 <- Sub(Info$MSEproj, MPs=input$TO_MP_select)
      TradeOff_plot2(MSEproj2)
    },
    width=function() {
      dims <- window_dims()
      dims[1]*0.3
    },
    height=function() {
      dims <- window_dims()
      dims[1]*0.3
    })

    output$tradeoff <- renderUI({
      ns <- NS(id)
      if(is.null(Info$MSEproj)) {
        return(h4('Projections have not been run yet.', style = "color:red"))
      } else {
        tagList(
          tabsetPanel(
            tabPanel(h4('Trade-Off Plots', style='color:black;'),
                     fluidRow(
                       column(9,
                              fluidRow(
                              plotOutput(session$ns('TradeOff_plot_group'),
                                                    height=TO_height())

                            ),

                     ),
                     column(3,
                            h3('Trade-Off Plot'),
                            checkboxGroupInput(session$ns('TO_MP_select'),
                                               label='Filter MPs',
                                               selected=Info$MSEproj@MPs,
                                               choices=Info$MSEproj@MPs)
                     )
                     )
            ),
            tabPanel(h4('Custom Plot', style='color:black;'),
                     fluidRow(
                     column(9,
                              plotOutput(session$ns('TradeOff_plot'))

                            ),
                     column(3,
                            h3('Trade-Off Plot'),
                            checkboxGroupInput(session$ns('TO2_MP_select'),
                                               label='Filter MPs',
                                               selected=Info$MSEproj@MPs,
                                               choices=Info$MSEproj@MPs),

                            h3('X-Axis'),
                            htmlOutput(session$ns('Xax')),
                            htmlOutput(session$ns('Xopt')),

                            h3('Y-Axis'),
                            htmlOutput(session$ns('Yax')),
                            htmlOutput(session$ns('Yopt'))
                     )
                     )

            )
          )
        )


      }

    })



    output$Xax <- renderUI({
      ns <- NS(id)
      if (class(Info$MSEproj) == 'MSE') {
        Yr1 <- Current_Year + 1
        Yr2 <- Current_Year + Info$MSEproj@proyears

        tagList(
          selectInput(session$ns('x_var'),
                      label = 'Variable',
                      choices = c('SB/SB0', 'SB/SBMSY', 'Catch', 'AAVY', 'AAVE')),
          sliderInput(session$ns('x_year'),
                      label = h4("Years"),
                      min = Yr1,
                      max = Yr2,
                      value = c(Yr1, Yr2),
                      step=1,
                      sep=''),
          selectInput(session$ns('x_metric'),
                      label = 'Metric',
                      choices = c('Median', 'Probability'))
        )
      }
    })

    output$Xopt <- renderUI({
      ns <- NS(id)
      if (class(Info$MSEproj) == 'MSE') {
        if (length(input$x_metric)>0) {
          if (input$x_metric == 'Median') {
            return(tagList(
              checkboxInput(session$ns('x_err'), 'Include error bars?',
                            value=FALSE)
            ))
          }
          if(input$x_metric == 'Probability') {
            return(tagList(
              numericInput(session$ns('x_reference'), 'Reference Value',
                           min=0,
                           value=0.5,
                           step=0.1)
            ))
          }
        }
      }
    })

    output$Yax <- renderUI({
      ns <- NS(id)
      if (class(Info$MSEproj) == 'MSE') {
        Yr1 <- Current_Year + 1
        Yr2 <- Current_Year + Info$MSEproj@proyears

        tagList(
          selectInput(session$ns('y_var'),
                      label = 'Variable',
                      choices = c('SB/SB0', 'SB/SBMSY', 'Catch', 'AAVY', 'AAVE'),
                      selected='Catch'),
          sliderInput(session$ns('y_year'),
                      label = h4("Years"),
                      min = Yr1,
                      max = Yr2,
                      value = c(Yr1, Yr2),
                      step=1,
                      sep=''),
          selectInput(session$ns('y_metric'),
                      label = 'Metric',
                      choices = c('Median', 'Probability'))
        )
      }
    })

    output$Yopt <- renderUI({
      ns <- NS(id)
      if (class(Info$MSEproj) == 'MSE') {
        if (length(input$y_metric)>0) {
          if (input$y_metric == 'Median') {
            return(tagList(
              checkboxInput(session$ns('y_err'), 'Include error bars?',
                            value=FALSE)
            ))
          }
          if(input$y_metric == 'Probability') {
            return(tagList(
              numericInput(session$ns('y_reference'), 'Reference Value',
                           min=0,
                           value=0.5,
                           step=0.1)
            ))
          }
        }
      }
    })



  })
}


