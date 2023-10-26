library(shiny)
library(bs4Dash)
library(tidyverse)
library(DT)
library(skimr)
library(shinyWidgets)

# Define maximum size to upload
options(shiny.maxRequestSize = 50 * 1024^2)


# Define UI module for the metadata
metadata_UI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(12,
             box(title = "Description of the Dataset",
                 status = "white",
                 solidHeader = TRUE,
                 collapsible = TRUE,
                 elevation = 3,
                 width = 12,
                 collapsed = F,
                 dataTableOutput(ns("meta"))),
             box(title = "Description of the Data (Metadata)",
                 status = "white",
                 solidHeader = TRUE,
                 collapsible = TRUE,
                 elevation = 3,
                 width = 12,
                 collapsed = T,
                 dataTableOutput(ns("describe")))
      )
    )
  )
}

# Define server module for the metadata
metadata_Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {

      output$meta <- renderDataTable({

        req(input$dataset)

        path <- file.path("data",paste0("A100_README_Cant et al_", input$dataset, "_1997_20210922.txt.csv"))

        metadata <- read.table(text = gsub("=", ":", readLines(input$dataset$datapath)), sep = ":")

        datatable(metadata [1:7, ],
                  rownames = F,
                  colnames = c("Keys", "Values"),
                  options = list(
                    dom = "tp",
                    autoWidth = F,
                    scrollX = TRUE))

      })
    }
  )
}

# Define UI module to explore the data
data_UI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(12,
             box(title = "Raw Data",
                 status = "white",
                 solidHeader = TRUE,
                 collapsible = TRUE,
                 elevation = 3,
                 width = 12,
                 collapsed = F,
                 dataTableOutput(ns("rawtable"))),
             box(title = "Summary statistics",
                 status = "white",
                 solidHeader = TRUE,
                 collapsible = TRUE,
                 elevation = 3,
                 width = 12,
                 collapsed = T,
                 verbatimTextOutput(ns("summary")))
      )
    )
  )
}

# Define server module to explore the data
data_Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      data <- reactive({
        req(input$upload2)
        inFile <- input$upload2
        if (is.null(inFile)) return(NULL)
        df<-read.csv(inFile$datapath, sep = ",", header = input$header)
        df <-df %>%
          mutate(across(input$range, as.factor))

        ## update input

        updateSelectInput(session,
                          inputId = 'Xvar',
                          label = 'X',
                          choices = colnames(df))
        updateSelectInput(session,
                          inputId = 'Yvar',
                          label = 'Y',
                          choices = colnames(df))

        return(df)

      })

      #output raw data
      output$rawtable <- renderDataTable({

        datatable(data(),
                  rownames = F,
                  options = list(
                    dom = "tp",
                    autoWidth = F,
                    scrollX = T))
      })


      #output summary statistics
      output$summary <- renderPrint({
        skim(data())
      })
    }
  )
}

# Define UI module for the plot
plot_UI <- function(id) {
  ns <- NS(id)
  tagList(
    # The plot tab with option to select X and Y variable.
    #The X and Y will be updated after loading the data
    fluidRow(
      column(3,
             box(title = "Select Plot Type",
                 status = "white",
                 solidHeader = TRUE,
                 collapsible = F,
                 elevation = 3,
                 width = 12,
                 awesomeRadio(
                   inputId = ns("button"),
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
                 selectInput(ns("Xvar"),
                             "X",
                             choices = ""),
                 selectInput(ns("Yvar"),
                             "Y",
                             choices =""))),
      column(9,
             box(title = "Plot Area",
                 status = "white",
                 solidHeader = TRUE,
                 collapsible = F,
                 elevation = 3,
                 width = 12,
                 plotOutput(ns("plot"))
             )
      )
    )
  )
}

# Define server module for the plot
plot_Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {

    }
  )
}

# UI interface for the explorer App
ui <- dashboardPage(
  skin = "lightblue",
  scrollToTop = TRUE,
  fullscreen = T,
  dark = NULL,

  # define the dashboard header
  header = dashboardHeader(title = "Menu",
                           status = "white",
                           h3("Reusable data explorer")),

  # define the dashboard sidebar
  sidebar = dashboardSidebar(
    collapsed = F,
    minified = F,
    skin = "light",
    status = "lightblue",
    elevation = 1,
    sidebarMenu(
      id = "sidebar",
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

   # Define the dashboard body
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
          strong("Explore yborealis "),
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
               tabPanel("Metadata",
                        metadata_UI("example")),
               tabPanel("Data",
                        data_UI("example")),
               tabPanel("Plot",
                        plot_UI("example")
               )
        )
      ),

      tabItem(tabName = "owndata",
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
                                                       selected = ",")
                                       ),
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
                              metadata_UI("owndata")),
                     tabPanel("Data",
                              data_UI("owndata")),
                     tabPanel("Plot",
                              plot_UI("owndata")
                     )

              )
      )
    )
  )
)





# Server logic for the explorer app
server <- function(input, output, session) {
  metadata_Server("example")
}


shinyApp(ui, server)
