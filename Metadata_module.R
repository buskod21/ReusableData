MetaUI <- function(id) {
  ns <- NS(id)
  tagList(
    # Data and metadata tab
    fluidRow(
      column(12,
             box(title = "Description of the Dataset",
                 status = "white",
                 solidHeader = TRUE,
                 collapsible = TRUE,
                 elevation = 3,
                 width = 12,
                 collapsed = F,
                 dataTableOutput(ns("meta"))
             ),
             box(title = "Data schema",
                 status = "white",
                 solidHeader = TRUE,
                 collapsible = TRUE,
                 elevation = 3,
                 width = 12,
                 collapsed = T,
                 dataTableOutput(ns("schema"))
             )
      )
    )
  )
}

MetaServer <- function(id, exampleMeta) {
  moduleServer(
    id,
    function(input, output, session) {
      ## Reactive function to load and create in-built Metadata

      CreateMetaData <- function(nameD, Yeard, DoU) {
        if (DoU == FALSE){
          filename <- reactive({
            list.files("data", pattern = paste0(nameD, exampleMeta(), Yeard))
          })
          return(reactive({
            read.table(text = gsub("=", ":", readLines(file.path("data",filename()))),sep =  ":")
          }))
        } else {
          return(reactive({
            UserMeta <- read.table(
              text = gsub("=", ":", readLines(uploadMeta())),
              sep = ":")
          }))

        }
      }
      # Create metadata based on the selected dataset and options
      metadata <- CreateMetaData("al_cow", "_1997", FALSE)

      # Render a data table for metadata
      output$meta <- renderDataTable({
        req(metadata())  # Ensure that metadata is available
        datatable(metadata()[1:7,],
                  rownames = FALSE,
                  colnames = c("Keys", "Values"),
                  options = list(dom = "tp",
                                 autoWidth = FALSE,
                                 scrollX = TRUE)
        )
      })

      # Render a data table for data schema
      output$schema <- renderDataTable({
        req(metadata())  # Ensure that metadata is available
        datatable(metadata()[8:13,],
                  rownames = FALSE,
                  colnames = c("Keys", "Values"),
                  options = list(dom = "tp",
                                 autoWidth = FALSE,
                                 scrollX = TRUE)
        )
      })

    }
  )
}


