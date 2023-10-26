library(shiny)
library(bs4Dash)
library(tidyverse)
library(DT)
library(plotly)
library(skimr)
library(shinyWidgets)

options(shiny.maxRequestSize = 10 * 1024^2)

source("tabItem_module.R")
source("Metadata_module.R")
source("Data_module.R")
source("Plot_module.R")
source("UI_module.R")


# Create a Shiny dashboard UI
ui <- dashboardPage(
  skin = "lightblue",  # Set the dashboard's skin color
  scrollToTop = TRUE,  # Enable scroll to top button
  fullscreen = TRUE,  # Enable fullscreen mode

  # Define the dashboard header
  dashboardHeader(
    title = "Menu",  # Set the title of the header
    status = "white",  # Set the header's status color
    h3("Reusable data Explorer")  # Add a subheading
  ),

  # Define the dashboard sidebar
  dashboardSidebar(
    collapsed = F,  # Sidebar is expanded by default
    minified = F,  # Sidebar is not minified
    skin = "light",
    status = "lightblue",
    elevation = 1,

    # Define the sidebar menu items
    sidebarMenu(
      menuItem("Home",  # Sidebar menu item for the home tab
               icon = icon("house-user"),
               tabName = "home"),

      menuItem("Data Explorer",  # Sidebar menu item for the data tab
               icon = icon("table"),
               tabName = "data"),

      menuItem("Explore borealis",  # Sidebar menu item for the owndata tab
               icon = icon("server"),
               tabName = "owndata")
    )
  ),

  # Define the dashboard body
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "home",  # Content for the "home" tab
        h5("Welcome to the reusable data explorer App."),  # Subheading
        br(),

        img(src = 'workshop1.jpeg', height=400, width=700),  # Display an image

        br(),

        p("This shiny web application was developed as a part of the Reusable
        research data made shiny workshop that was
        held at the University of Guelph, Guelph Ontario Canada.
          You can find more information about the workshop and access the workshop materials",
          tags$a(href = "https://github.com/agrifooddatacanada/RRDMS_Workshop",
                 "here.", target = "_blank")),  # Create a hyperlink

        h5("About the App"),

        p(" The ", strong("Data explorer tab "), "demonstrates how the app works.
                          It displays the Metadata, raw data, some summary statistics
                          and plots of data gotten from an already published article.",
          tags$a(href = "https://doi.org/10.4141/A95-125",
                 "Click here for the publication.",
                 target = "_blank")),  # Create a hyperlink

        p("Finally, the ",
          strong("Explore borealis "),
          "allows you to explore data gotten from the Borealis database.
           After downloading the data into your local computer,
           the data explorer app allows you to upload the data into the app
           where you can view the Metadata, raw data,
           get some summary statistics and visualize some of the results in boxplot or scatter plot.",
          tags$a(href = "https://doi.org/10.5683/SP3/WVC09T",
                 "Click here for the Borealis database.",
                 target = "_blank")),  # Create a hyperlink
        p(h5("Have fun exploring reusable data!")
        )
      ),
      tabItem(
        tabName = "data",  # Content for the "data" tab
        selectInput("dataset",
                    "Select a Dataset",
                    choices = c("Fatty Acid" = "FattyAcid",
                                "Milk" = "Milk",
                                "Feed" = "Feed"
                    ),
                    selected = "FattyAcid"
        ),
        tabItemUI("Example")  # Include a UI element for the "Example" tab
      ),

      tabItem(
        tabName = "owndata",  # Content for the "owndata" tab
        fileInput('upload', 'Data file',
                  placeholder = "No file selected",
                  accept=c('text/csv',
                           'text/comma-separated-values,text/plain',
                           '.csv')),  # File input widget
        # Input: Checkbox if file has header ----
        checkboxInput("header", "Header", TRUE),  # Checkbox for header
        # Quotes
        selectInput(inputId = "upload2_quote",
                    label =  "Quotes",
                    choices = c('None' = "NULL",
                                'Double Quote' = '"',
                                'Single Quote' = "'"),
                    selected = "None"
        ),
        tabItemUI("Borealis")  # Include a UI element for the "Borealis" tab
      )
    )
  )
)

server <- function(input, output, session) {

  exampleMeta <- reactive({
    input$dataset
  })

  # Input and read data

  tabItemServer("Example", exampleMeta())

}




shinyApp(ui, server)
