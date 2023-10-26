PlotUI <- function(id) {
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
                   inputId = "button",
                   label = "Plot type",
                   choices = c("Boxplot","Scatter plot"),
                   selected = "Boxplot",
                   inline = TRUE
                 )
             ),

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
                             choices ="")
             )
      ),
      column(9,
             box(title = "Plot Area",
                 status = "white",
                 solidHeader = TRUE,
                 collapsible = F,
                 elevation = 3,
                 width = 12,
                 plotOutput(ns("plot")
                 )
             )
      )
    )
  )
}

PlotServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {

    }
  )
}
