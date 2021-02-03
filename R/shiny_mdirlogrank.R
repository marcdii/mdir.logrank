#' A shiny app for the package mdir.logrank
#'
#' This function provides a shiny app for calculating the
#' multiple-direction logrank test for the two-sided and the one-sided
#' testing problem.
#'
#'
#' @aliases mdir.logrank.shiny
#'
#' @import shiny
#' @import shinyjs
#' @import shinyWidgets
#' @import tippy
#'
#'
#'
#' @export

mdir.logrank.shiny <- function() {
  requireNamespace("shiny", quietly = TRUE)
  if (!("package:shiny" %in% search())) {
    attachNamespace("shiny")
  }



  ui <- fluidPage(theme = shinythemes::shinytheme("cerulean"),
                  shinyjs::useShinyjs(),
                  titlePanel("Multiple-direction logrank test"),
                  sidebarLayout(
                    sidebarPanel(
                      splitLayout(
                        fileInput("infile", "Choose CSV File",
                                  accept = c(
                                    "text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv")),
                        checkboxInput("header", "Header", TRUE),
                        selectInput("sep","Seperator in csv", c(",",
                                                                ";",
                                                                ".",
                                                                "|"))

                      ),

                      tags$head(tags$style(HTML("
                                                .shiny-split-layout > div {
                                                overflow: visible;
                                                }
                                                "))), #for selectinput in splitlayout with full dropdown view


                      #remove arrows in numericInputs
                      tags$style(HTML("
                                 input[type=number] {
                                                              -moz-appearance:textfield;
                                                    }
                                  input[type=number]::{
                                                  -moz-appearance:textfield;
                                                    }
                        input[type=number]::-webkit-outer-spin-button,
                        input[type=number]::-webkit-inner-spin-button {
                        -webkit-appearance: none;
                        margin: 0;
                        }
                        ")),



                      h3(id="titleLoadData","Load dataset first!", style = "color:red"),


                      shinyjs::hidden(
                      selectInput("Method", "Choose between the one- sided and the two-sided testing problem:",
                                  c("one-sided (inferiority of the first group)" = "one.sided",
                                    "two-sided"= "two.sided"))
                      ),

                      splitLayout(cellWidths = c("40%","40%"),

                        uiOutput(outputId = 'dynamicInputEvent'),
                        uiOutput(outputId = 'dynamicInputEventLabel')

                      ),

                      splitLayout(cellWidths = c("40%","40%"),
                        uiOutput(outputId = 'dynamicInputTime')
                      ),


                      splitLayout(cellWidths = c("40%","40%"),

                        uiOutput(outputId = 'dynamicInput'),
                        uiOutput(outputId = 'dynamicInput2')

                      ),

                      shinyjs::hidden(
                      h5(id="titleWeights",strong("Which weight functions w should be combined?"), style = "color:grey")
                      ),


                        splitLayout(cellWidths = c("30%","60%","10%"),
                              shinyjs::hidden(
                                  checkboxGroupInput("Weights", "Pre-specified weights",selected = c("crossing","proportional"),
                                                     choiceNames = list("Proportional", "Early","Late","Central"),
                                                     choiceValues = list("proportional", "early","late","central"))
                              ),
                              shinyjs::hidden(
                                selectInput("weights1","Specify the exponents (r,g) of weights  w(x) = x^r(1-x)^g ",
                                            paste0("(",expand.grid(0:10,0:10)[,1],",",expand.grid(0:10,0:10)[,2],")"),
                                            multiple=TRUE,selectize = TRUE)

                        )
                      ),



                      splitLayout(cellWidths = c("40%","40%","20%"),
                        shinyjs::hidden(
                            numericInput("nboot", "Number of bootstrap iterations", value = 1000)
                        ),
                        shinyjs::hidden(
                               selectInput("bootstrapweights", "Select wild bootstrap type:",
                                    c("Rademacher" = "rade",
                                      "Normal"= "norm",
                                      "Poisson"="pois"))
                        )
                      ),

                      splitLayout(cellWidths = c("40%","40%","20%"),
                          shinyjs::hidden(
                                  numericInput("nperm", "Number of permuation iterations", value = 1000)
                          )
                      ),
                      shinyjs::hidden(
                        checkboxInput("plots", "Plot the surival curves", FALSE)
                      ),


                      shinyjs::hidden(
                        actionButton("process", "Calculate", class = "btn-primary")
                      )
                      , width = 6
                      ),




                    mainPanel(

                      verbatimTextOutput("result"),
                      plotOutput("result_plot"),
                      width = 6


                    )
                      )
                      )


  server <- function(input, output,session) {

    datasetInput <- reactive({

      req(input$infile)

      if (is.null(input$infile))
        return(NULL)
      read.csv(input$infile$datapath, header = input$header, sep = as.character(input$sep))

    })


    observeEvent(input$infile, {

      if(is.null(input$infile)){
        shinyjs::hide(id = "Method")
        shinyjs::hide(id = "weights1")
        shinyjs::hide(id = "Weights")
        shinyjs::hide(id = "WeightsTwo")
        shinyjs::hide(id = "titleWeights")
        shinyjs::hide(id = "nboot")
        shinyjs::hide(id = "nperm")
        shinyjs::hide(id = "bootstrapweights")
        shinyjs::hide(id = "process")
        shinyjs::hide(id = "plots")

      }else{

        shinyjs::show(id = "Method")
        shinyjs::show(id = "weights1")
        shinyjs::show(id = "Weights")
        shinyjs::show(id = "titleWeights")
        shinyjs::show(id = "plots")


        observeEvent(input$Method, {

          if (input$Method == "one.sided") {
            shinyjs::show("nboot")
            shinyjs::show("bootstrapweights")
            shinyjs::hide("nperm")
            updateCheckboxGroupInput(session, "Weights",selected =c("proportional","early","late"),
                                     choiceNames = list("Proportional", "Early","Late","Central"),
                                     choiceValues = list("proportional", "early","late","central"))

          }

          if (input$Method == "two.sided") {
            shinyjs::show("nperm")
            shinyjs::hide("nboot")
            shinyjs::hide("bootstrapweights")
            updateCheckboxGroupInput(session, "Weights",selected = c("crossing","proportional"),
                                     choiceNames = list("Proportional", "Crossing"),
                                     choiceValues = list("proportional", "crossing"))
          }

        })
        shinyjs::show(id = "process")
        shinyjs::hide(id = "titleLoadData")

      }
    })




    values <- reactiveValues()

    output$dynamicInput <- renderUI({
      if (input$Method == "one.sided" || input$Method == "two.sided") {

      # This input exists if the `static`
      # one is equal to `A` only
        selectInput(inputId = 'dynamic',
                    label = "Group variable",
                    choices = colnames(datasetInput()))
      } else {
        return(NULL)}

    })
    ## this bit fixes the issue

    observe({
      if  (input$Method == "one.sided" || input$Method == "two.sided") {
        values$dyn <- input$dynamic
      } else {
        values$dyn <- NULL
      }
    })






    values2 <- reactiveValues()

    output$dynamicInput2 <- renderUI({
      if (input$Method == "one.sided") {

        # This input exists if the `static`
        # one is equal to `A` only
        selectInput(inputId = 'dynamic2',
                    label = "Name of first group",
                    choices = unique(datasetInput()[,values$dyn]))
      } else {
        return(NULL)}

    })
    ## this bit fixes the issue

    observe({
      if  (input$Method == "one.sided") {
        values2$dyn <- input$dynamic2
      } else {
        values2$dyn <- NULL
      }
    })




    #time

    valuesTime <- reactiveValues()
    output$dynamicInputTime <- renderUI({
      if (input$Method == "one.sided" || input$Method == "two.sided") {

        # This input exists if the `static`
        # one is equal to `A` only
        selectInput(inputId = 'dynamicTime',
                    label = "Time variable",
                    choices = colnames(datasetInput()))
      } else {
        return(NULL)}

    })
    ## this bit fixes the issue

    observe({
      if  (input$Method == "one.sided" || input$Method == "two.sided") {
        valuesTime$dyn <- input$dynamicTime
      } else {
        valuesTime$dyn <- NULL
      }
    })


    #event
    valuesEvent <- reactiveValues()

    output$dynamicInputEvent <- renderUI({
      if (input$Method == "one.sided" || input$Method == "two.sided") {

        # This input exists if the `static`
        # one is equal to `A` only
        selectInput(inputId = 'dynamicEvent',
                    label = "Event/ censoring variable",
                    choices = colnames(datasetInput()))
      } else {
        return(NULL)}

    })
    ## this bit fixes the issue

    observe({
      if  (input$Method == "one.sided" || input$Method == "two.sided") {
        valuesEvent$dyn <- input$dynamicEvent
      } else {
        valuesEvent$dyn <- NULL
      }
    })


    valuesEventLabel <- reactiveValues()

    output$dynamicInputEventLabel <- renderUI({
      if (input$Method == "one.sided" || input$Method == "two.sided")  {

        # This input exists if the `static`
        # one is equal to `A` only
        selectInput(inputId = 'dynamicEventLabel',
                    label = "Label of censored variable",
                    choices = unique(datasetInput()[,valuesEvent$dyn]))
      } else {
        return(NULL)}

    })
    ## this bit fixes the issue

    observe({
      if (input$Method == "one.sided" || input$Method == "two.sided")  {
        valuesEventLabel$dyn <- input$valuesEventLabel
      } else {
        valuesEventLabel$dyn <- NULL
      }
    })








    observeEvent(input$process, {



        if (input$Method == "one.sided" ){
          data <- as.data.frame(datasetInput())
          data_cal <- data[,c(input$dynamicEvent, input$dynamicTime, input$dynamic)]
          rg <- list()
          givenWeights <- isolate(input$Weights)
          if("proportional" %in% givenWeights){
            rg[[length(rg)+1]] <- c(0,0)
          }
          if("early" %in% givenWeights){
            rg[[length(rg)+1]] <- c(0,4)
          }
          if("late" %in% givenWeights){
            rg[[length(rg)+1]] <- c(4,0)
          }
          if("central" %in% givenWeights){
            rg[[length(rg)+1]] <- c(1,1)
          }
          inputWeights <- isolate(input$weights1)

          if(is.null(inputWeights)){}else{
            expand.grid(0:10,0:10)
            kombiAll <- paste0("(",expand.grid(0:10,0:10)[,1],",",expand.grid(0:10,0:10)[,2],")")
            kombi <- expand.grid(0:10,0:10)[which(kombiAll %in% inputWeights),]

            for (i in 1:(dim(kombi)[1])){
              rg[[length(rg)+1]] <- as.numeric(kombi[i,])
            }
        }
          if(length(rg)==0){
            rg = list(c(0,0))
          }

          output_one <-   mdir.onesided(data= isolate(data_cal),
                                        group1 = isolate(input$dynamic2),
                                        rg = isolate(rg),
                                        wild = isolate(input$bootstrapweights),
                                        iter = isolate(input$nboot)
          )

          output$result <- renderPrint({
            output_one
          })

          if(input$plots){
            output$result_plot <- renderPlot({
              plot(output_one)
            })
          }

        }

      if (input$Method == "two.sided" ){
        data <- as.data.frame(datasetInput())
        data_cal <- data[,c(input$dynamicEvent, input$dynamicTime, input$dynamic)]
        rg <- list()
        givenWeights <- isolate(input$Weights)
        if("proportional" %in% givenWeights){
          rg[[length(rg)+1]] <- c(0,0)
        }
        if("crossing" %in% givenWeights){
          crossing <- TRUE
         } else {
           crossing <- FALSE
         }

        inputWeights <- isolate(input$weights1)

        if(is.null(inputWeights)){}else{
          expand.grid(0:10,0:10)
          kombiAll <- paste0("(",expand.grid(0:10,0:10)[,1],",",expand.grid(0:10,0:10)[,2],")")
          kombi <- expand.grid(0:10,0:10)[which(kombiAll %in% inputWeights),]

          for (i in 1:(dim(kombi)[1])){
            rg[[length(rg)+1]] <- as.numeric(kombi[i,])
          }
        }

        if(length(rg)==0){
          rg = list(c(0,0))
        }
       output_two <-  mdir.logrank(data= isolate(data_cal),
                     cross = crossing,
                     rg = isolate(rg),
                     nperm = isolate(input$nperm)
                     )

        output$result <- renderPrint({
          output_two
        })

        if(input$plots){
          output$result_plot <- renderPlot({
            plot(output_two)
          })
        }

      }

    }
    ) #end of observeEvent(input$process

  }


  shinyApp(ui = ui, server = server)

}
