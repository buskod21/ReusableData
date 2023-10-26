DataUI <- function(id) {
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

DataServer <- function(id, exampleMeta) {
  moduleServer(
    id,
    function(input, output, session) {
      # Render and output raw data and summary

      ## Input and read data

      data <- reactive({
        req(exampleMeta())
        path <- file.path("data",paste0("cow", exampleMeta(), "Cant1997.csv"))

        df <- read.csv(path,
                       sep = ",",
                       header = T
        )
        df <- df %>%
          mutate(across(1:5, as.factor))


        return(df)
      })


      output$rawtable <- OutputTableRender(data())

      #output summary statistics
      output$summary <- renderPrint({
        skim(data())
      })

    }
  )
}
