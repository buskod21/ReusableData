library(shiny)
library(bs4Dash)
library(tidyverse)
library(DT)
library(skimr)
library(shinyWidgets)

options(shiny.maxRequestSize = 10 * 1024^2)



ui <- dashboardPage(
  skin = "lightblue",
  scrollToTop = TRUE,
  fullscreen = T,
  dark = NULL,

  # design the dashboard header
  dashboardHeader(title = "Menu",
                           status = "white",
                           h3("Reusable data explorer")),

  dashboardSidebar(
    collapsed = F,
    minified = F,
    skin = "light",
    status = "lightblue",
    elevation = 1,
    sidebarMenu(
      menuItem("Home",
               icon = icon("house-user"),
               tabName = "home"),

      menuItem("Data Explorer",
               icon = icon("table"),
               tabName = "data"),

      menuItem("Explore borealis",
               icon = icon("server"),
               tabName = "owndata")
    )
  ),

  body = dashboardBody(
    tabItems(
      tabItem(
        tabName = "home",
        h5("Welcome to the reusable data explorer App!"),
        br(),

        img(src = 'workshop1.jpeg', height=400, width=700),

        br(),

        p("This shiny web application was developed as a part of the Reusable
        research data made shiny workshop that was
        held at the University of Guelph, Guelph Ontario Canada.
          You can find more information about the workshop and acess the workshop materials",
          tags$a(href = "https://github.com/agrifooddatacanada/RRDMS_Workshop",
                 "here.", target = "_blank")),

        h5("About the App"),

        p(" The ", strong("Data explorer tab "), "demonstrates how the app works.
                          It displays the Metadata, raw data, some summary statistics
                          and plots of data gotten from an already published article.",
          tags$a(href = "https://doi.org/10.4141/A95-125",
                 "Click here for the publication.",
                 target = "_blank")),

        p("Finally, the ",
          strong("Explore borealis "),
          "allows you to explore data gotten from the Borealis database.
           After downloading the data into your local computer,
           the data explorer app allows you to upload the data into the app
           where you can view the Metadata, raw data,
           get some summary statistics and visualize some of the results in boxplot or scatter plot.",
          tags$a(href = "https://doi.org/10.5683/SP3/WVC09T",
                 "Click here for the Borealis database.",
                 target = "_blank")),
        p(h5("Have fun exploring reusable data!"))
      ),

      tabItem(
        tabName = "data",
        fluidRow(
          column(12,
                 column(12,
                        selectInput(inputId = "dataset",
                                    label = "Choose a dataset",
                                    choices = c("Fatty Acid" = "FattyAcid",
                                                "Milk" = "Milk",
                                                "Feed" = "Feed"
                                    ),
                                    selected = "FattyAcid"
                        )
                 )
          )
        ),
        tabBox(id = "tabfatty",
               title = "",
               width = 12,
               collapsible = T,
               maximizable = T,
               elevation = 1,
               solidHeader = T,
               status = "lightblue",
               side = "right",
               type = "tabs",


               # Data and metadata tab
               tabPanel("Metadata",
                        fluidRow(
                          column(12,
                                 box(title = "Description of the Dataset",
                                     status = "white",
                                     solidHeader = TRUE,
                                     collapsible = TRUE,
                                     elevation = 3,
                                     width = 12,
                                     collapsed = F,
                                     dataTableOutput("meta1")),
                                 box(title = "Description of the Data (Metadata)",
                                     status = "white",
                                     solidHeader = TRUE,
                                     collapsible = TRUE,
                                     elevation = 3,
                                     width = 12,
                                     collapsed = T,
                                     dataTableOutput("meta"))
                          )
                        )),
               tabPanel("Data",
                        fluidRow(
                          column(12,
                                 box(title = "Raw Data",
                                     status = "white",
                                     solidHeader = TRUE,
                                     collapsible = TRUE,
                                     elevation = 3,
                                     width = 12,
                                     collapsed = F,
                                     dataTableOutput("rawtable")),
                                 box(title = "Summary statistics",
                                     status = "white",
                                     solidHeader = TRUE,
                                     collapsible = TRUE,
                                     elevation = 3,
                                     width = 12,
                                     collapsed = T,
                                     verbatimTextOutput("summary"))
                          )
                        )
               ),

               # The plot tab with option to select X and Y variable.
               #The X and Y will be updated after loading the data
               tabPanel("Plot",
                        fluidRow(
                          column(3,
                                 box(title = "Select Plot Type",
                                     status = "white",
                                     solidHeader = TRUE,
                                     collapsible = F,
                                     elevation = 3,
                                     width = 12,
                                     awesomeRadio(
                                       inputId = "button",
                                       label = "Plot type",
                                       choices = c("Boxplot","Scatter plot"),
                                       selected = "Boxplot",
                                       inline = TRUE
                                     )),

                                 box(title = "Select Variables",
                                     status = "white",
                                     solidHeader = TRUE,
                                     collapsible = F,
                                     elevation = 3,
                                     width = 12,
                                     selectInput("Xvar",
                                                 "X",
                                                 choices = ""),
                                     selectInput("Yvar",
                                                 "Y",
                                                 choices =""))),
                          column(9,
                                 box(title = "Plot Area",
                                     status = "white",
                                     solidHeader = TRUE,
                                     collapsible = F,
                                     elevation = 3,
                                     width = 12,
                                     plotOutput("plot")))
                        )
               )
        )
      ),

      tabItem(tabName ="owndata",
              tabBox(id = "tabowndata",
                     title = "",
                     width = 12,
                     collapsible = T,
                     maximizable = T,
                     elevation = 1,
                     solidHeader = T,
                     status = "lightblue",
                     side = "right",
                     type = "tabs",

                     tabPanel("Upload data",
                              icon = icon("upload"),
                              fluidRow(
                                column(12,
                                       box(title = "Upload Metadata here",
                                           status = "white",
                                           solidHeader = TRUE,
                                           collapsible = TRUE,
                                           elevation = 3,
                                           width = 6,
                                           # Input metadata
                                           fileInput('upload1', 'Metadata',
                                                     placeholder = "No file selected",
                                                     accept=c('text/csv',
                                                              'text/comma-separated-values,text/plain',
                                                              '.csv')),

                                           # Input: Checkbox if file has header ----
                                           checkboxInput("header", "Header", TRUE),

                                           #Select File delimiter
                                           selectInput(inputId = "sep_heatmap",
                                                       label = "File Delimiter",
                                                       choices = c(Comma = ",",
                                                                   Semicolon = ";",
                                                                   Tab = "\t",
                                                                   Space = " "),
                                                       selected = ",")),

                                       box(title = "Upload data file here",
                                           status = "white",
                                           solidHeader = TRUE,
                                           collapsible = TRUE,
                                           collapsed = TRUE,
                                           elevation = 3,
                                           width = 6,

                                           # input raw data
                                           fileInput('upload2', 'data file',
                                                     placeholder = "No file selected",
                                                     accept=c('text/csv',
                                                              'text/comma-separated-values,text/plain',
                                                              '.csv')),
                                           # Input: Checkbox if file has header ----
                                           checkboxInput("header", "Header", TRUE),
                                           # Quotes
                                           selectInput(inputId = "upload2_quote",
                                                       label =  "Quotes",
                                                       choices = c('None' = "NULL",
                                                                   'Double Quote' = '"',
                                                                   'Single Quote' = "'"),
                                                       selected = "None"
                                           ),
                                           sliderInput(inputId = "range",
                                                       label = "Independent variables",
                                                       min = 1, max = 10, value = c(1,2),
                                           )

                                       )
                                )
                              )

                     ),

                     tabPanel("Metadata",
                              fluidRow(
                                column(12,
                                       box(title = "View metadata",
                                           status = "white",
                                           solidHeader = TRUE,
                                           collapsible = TRUE,
                                           elevation = 3,
                                           width = 12,
                                           dataTableOutput("ownmeta"))
                                )

                              )

                     ),
                     # Data tab
                     tabPanel("Data",
                              fluidRow( #
                                column(12, #
                                       box(title = "Raw Data",
                                           status = "white",
                                           solidHeader = TRUE,
                                           collapsible = TRUE,
                                           elevation = 3,
                                           width = 12,
                                           collapsed = T,
                                           dataTableOutput("datatable")),
                                       box(title = "Summary statistics",
                                           status = "white",
                                           solidHeader = TRUE,
                                           collapsible = TRUE,
                                           elevation = 3,
                                           width = 12,
                                           collapsed = T,
                                           verbatimTextOutput("ownsummary"))
                                )
                              )
                     ),

                     # The plot tab with option to select X and Y variable.
                     #The X and Y will be updated after loading the data
                     tabPanel("Plot",
                              fluidRow(
                                column(3,
                                       box(title = "Select Plot Type",
                                           status = "white",
                                           solidHeader = TRUE,
                                           collapsible = F,
                                           elevation = 3,
                                           width = 12,
                                           awesomeRadio(
                                             inputId = "button1",
                                             label = "Plot type",
                                             choices = c("Boxplot","Scatter plot"),
                                             inline = TRUE
                                           )),
                                       box(title = "Select Variables",
                                           status = "white",
                                           solidHeader = TRUE,
                                           collapsible = F,
                                           elevation = 3,
                                           width = 12,
                                           selectInput(inputId ="Xvar1",
                                                       "X",
                                                       choices = ""),
                                           selectInput(inputId ="Yvar1",
                                                       "Y",
                                                       choices =""))),
                                column(9,
                                       box(title = "Plot Area",
                                           status = "white",
                                           solidHeader = TRUE,
                                           collapsible = F,
                                           elevation = 3,
                                           width = 12,
                                           plotOutput("ownplot")))
                              )
                     )
              )
      )

    )

  )
)







OutputTableMetaRender <- function(Data, MorP) {
  if (MorP == "P"){
    output <- renderDataTable({
      datatable(Data[1:7, ],
                rownames = F, # <------------- Don't show row numbers
                colnames = c("Keys", "Values"),
                options = list(
                  dom = "t",
                  autoWidth = FALSE,
                  scrollX = TRUE))
    })
    return (output)
  } else if (MorP == "M"){
    output <- renderDataTable({
      datatable(Data[8:13, ],
                rownames = F, # <------------- Don't show row numbers
                colnames = c("Keys", "Values"),
                options = list(
                  dom = "t",
                  autoWidth = FALSE,
                  scrollX = TRUE))
    })

    return (output)
  } else {
    output <- renderDataTable({
      datatable(Data,
                rownames = F, # <------------- Don't show row numbers
                colnames = c("Keys", "Values"),
                options = list(
                  dom = "tp",
                  autoWidth = FALSE,
                  scrollX = TRUE))
    })
  }
}
OutputTableRender <- function(Data) {
  output <- renderDataTable({
    datatable(Data,
              rownames = F,
              options = list(
                dom = "tp",
                autoWidth = F,
                scrollX = T))
  })

  return (output)
}
PlotRetrieve<- function(Data,input1,input2, sizez) {
  #output plot
  if (sizez == 1){
    x <-
      Data %>%
      ggplot(aes_string(x=input1, y=input2, group=input1, color= input1))+
      geom_boxplot(size=sizez) +
      theme_classic()
    return(x)
  } else if (sizez == 3){
    x <-
      Data %>%
      ggplot(aes_string(x=input1, y=input2, group=input1, color= input1))+
      geom_point(size=sizez) +
      theme_classic()
    return(x)
  }
}

server <- function(input, output, session) {

  DataIN <- reactive({
    input$dataset
  })
  DataINU <- reactive({
    input$upload1$datapath
  })

  CreateMetaData <- function(nameD, Yeard, DoU) {
    if (DoU == FALSE){
      filename <- reactive({
        list.files("data", pattern = paste0(nameD, DataIN(), Yeard))
      })
      return(reactive({
        read.table(text = gsub("=", ":", readLines(file.path("data",filename()))), sep =  ":")
      }))
    } else {
      return(reactive({
        metadata <- read.table(DataINU())
      }))
    }
  }

  ObserveInputData <- function(data){

    observe({
      req(data)

      ## update input
      updateSelectInput(session,
                        inputId = 'Xvar',
                        label = 'X',
                        choices = colnames(data))
    })

    observe({
      req(data)
      Ychoices <- subset(colnames(data), !(colnames(data) %in% input$Xvar))

      updateSelectInput(session,
                        inputId = 'Yvar',
                        label = 'Y',
                        choices = Ychoices)
    })
  }

  # Read metadata and output as a datatable
  metadata <- CreateMetaData("al_cow", "_1997", FALSE)
  output$meta1 <- OutputTableMetaRender(metadata(), "P")
  output$meta <- OutputTableMetaRender(metadata(), "M")

  # Input and read data
  data <- reactive({
    req(input$dataset)
    path <- file.path("data",paste0("cow", input$dataset, "Cant1997.csv"))
    df <- read.csv(path,sep = ",", header = T)
    df <- df %>% mutate(across(1:5, as.factor))

    return(df)
  })


  #output summary statistics
  output$summary <- renderPrint({
    skim(data())
  })









  In1 <- reactive({
    input$Xvar
  })
  In2 <- reactive({
    input$Yvar
  })
  plot1 <- reactive({
    PlotRetrieve(data(), In1(), In2(),1)
  })
  plot2 <- reactive({
    PlotRetrieve(data(), In1(), In2() ,3)
  })

  selectplot <- reactiveVal(TRUE)
  value <- reactiveVal(0)

  observeEvent(input$button, {
    selectplot(!selectplot())
    value(value() + 1)
  })
  select_graph <- reactive({
    if (selectplot() == TRUE && value() != 0) {
      plot2()
    } else if (selectplot() ==  FALSE && value() != 0){
      plot1()
    }
  })
  output$plot <- renderPlot({
    select_graph()
  })


  # This is the server portion for uploaded data
  # Read metadata and output as a datatable

  metadata1 <- CreateMetaData("", "", TRUE)
  output$ownmeta <- OutputTableMetaRender(metadata1(), "NULL")



  output$ownmeta <- renderDataTable({
    req(input$upload1)

    metadata <- read.table(
      text = gsub("=", ":", readLines(input$upload1$datapath)),
      sep = ":")

    datatable(metadata,
              rownames = F,
              colnames = c("Keys", "Values"),
              options = list(
                dom = "tp",
                autoWidth = F,
                scrollX = TRUE))

  })













  #output uploaded data
  # Input and read  uploaded data
  data1 <- reactive({
    req(input$upload2)
    inFile <- input$upload2
    range <- input$range
    if (is.null(inFile)) return(NULL)
    df<-read.csv(inFile$datapath, sep = ",", header = input$header)
    df <-df %>%
      mutate(across(input$range[1]:input$range[2], as.factor)) # still need to set factor range


    return(df)

  })
  ## update input after uploading data
  observe({
    req(data1())

    ## update input
    updateSelectInput(session,
                      inputId = 'Xvar1',
                      label = 'X',
                      choices = colnames(data1()))
  })

  observe({
    req(data1())
    Ychoices <- subset(colnames(data1()), !(colnames(data1()) %in% input$Xvar1))

    updateSelectInput(session,
                      inputId = 'Yvar1',
                      label = 'Y',
                      choices = Ychoices)
  })

  # output uploaded raw data
  output$datatable <- renderDataTable({

    datatable(data1(),
              rownames = F,
              options = list(
                dom = "tp",
                autoWidth = F,
                scrollX = T))
  })

  #output summary statistics for uploaded data
  output$ownsummary <- renderPrint({
    skim(data1())
  })

  #output selected plot type for uploaded data
  selectplot1 <- reactiveVal(TRUE)
  value1 <- reactiveVal(0)

  InO1 <- reactive({
    input$Xvar1
  })
  InO2 <- reactive({
    input$Yvar1
  })
  ownplot1 <- reactive({
    PlotRetrieve(data1(), InO1(), InO2(), 1)
  })
  ownplot2 <- reactive({
    PlotRetrieve(data1(), InO1(), InO2(), 3)
  })

  observeEvent(input$button1, {
    selectplot1(!selectplot1())
    value1(value1() + 1)
  })

  select_graph1 <- reactive({
    if (selectplot1() == TRUE && value1() != 0){
      ownplot2()
    } else if (selectplot1() ==  FALSE && value1() != 0){
      ownplot1()
    }
  })

  output$ownplot <- renderPlot({
    select_graph1()
  })
}

shinyApp(ui, server)
