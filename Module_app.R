library(shiny)
library(bs4Dash)
library(tidyverse)
library(DT)
library(skimr)
library(shinyWidgets)

options(shiny.maxRequestSize = 10 * 1024^2)

# Load datasets
fattyacid <- read.csv("data/cowFattyAcidCant1997.csv")
feed <- read.csv("data/cowFeedCant1997.csv")
milk <- read.csv("data/cowMilkCant1997.csv")

# Create a list for the data
datasets <- list(
  "Fatty acid" = fattyacid,
  "Feed" =feed ,
  "Milk"= milk
)


# Load Metadata
fattymeta <- read.table(text = gsub("=",
                                   ":",
                                   readLines("data/A100_README_Cant et al_cowFattyAcid_1997_20210922.txt")),
                       sep = ":")
Metafeed <- read.table(text = gsub("=",
                                    ":",
                                    readLines("data/B100_README_Cant et al_cowFeed_1997_20210922.txt")),
                        sep = ":")
MetaMilk <- read.table(text = gsub("=",
                                    ":",
                                    readLines("data/C100_README_Cant et al_cowMilk_1997_20210922.txt")),
                        sep = ":")

# Create a list of Metadata
Metadata <- list(
  "Fattyacid Metadata" = fattymeta,
  "Feed Metadata" =Metafeed ,
  "Milk Metadata"= MetaMilk
)

# Load the module file
source("Metadata_module.R")
source("Data_module.R")
source("Plot_module.R")

ui <- dashboardPage(
  skin = "lightblue",
  scrollToTop = TRUE,
  fullscreen = TRUE,

  dashboardHeader(title = "Menu",
                  status = "white",
                  h3("Reusable data Explorer")
  ),
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
  dashboardBody(
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
        p(h5("Have fun exploring reusable data!")
        )
      ),
      tabItem(
        tabName = "data",
        tabBox(title = "",
               width = 12,
               collapsible = T,
               maximizable = T,
               elevation = 1,
               solidHeader = T,
               status = "lightblue",
               side = "right",
               type = "tabs",
               tabPanel("Metadata",
                        MetaUI("example")),
               tabPanel("Data",
                        DataUI("example")),
               tabPanel("Plot",
                        PlotUI("example"))

        )
      ),

      tabItem(
        tabName = "owndata",
        tabBox(title = "",
               width = 12,
               collapsible = T,
               maximizable = T,
               elevation = 1,
               solidHeader = T,
               status = "lightblue",
               side = "right",
               type = "tabs",
               tabPanel(),
               tabPanel("Metadata",
                        MetaUI("owndata")),
               tabPanel("Data",
                        DataUI("owndata")),
               tabPanel("Plot",
                        PlotUI("owndata"))

        )
      )
    )
  )
)


server <- function(input, output, session) {
  user_meta <- reactive({
    req(input$user_uploaded_data)
    uploaded_data <- read.csv(input$user_uploaded_data$datapath)
    # Pre-process user-uploaded data as needed (e.g., data cleaning, transformation)
    return(uploaded_data)
  })

  in_built_meta <- reactive({
    # Load your in-built data here
    # Example:
    in_built_data <- read.csv("path/to/in-built/data.csv")
    # Pre-process in-built data as needed (e.g., data cleaning, transformation)
    return(in_built_data)
  })

  # Rest of your server code...
}




shinyApp(ui, server)
