# Define a custom UI function for a tab item
tabItemUI <- function(id) {
  ns <- NS(id)  # Create a namespace for the UI elements
  tagList(
    tabBox(title = "",
           width = 12,
           collapsible = TRUE,
           maximizable = TRUE,
           elevation = 1,
           solidHeader = TRUE,
           status = "lightblue",
           side = "right",
           type = "tabs",
           tabPanel("Metadata",
                    fluidRow(
                      column(12,
                             box(title = "Description of the Dataset",
                                 status = "white",
                                 solidHeader = TRUE,
                                 collapsible = TRUE,
                                 elevation = 3,
                                 width = 12,
                                 collapsed = FALSE,  # Metadata box is initially expanded
                                 dataTableOutput(ns("meta"))
                             ),
                             box(title = "Data schema",
                                 status = "white",
                                 solidHeader = TRUE,
                                 collapsible = TRUE,
                                 elevation = 3,
                                 width = 12,
                                 collapsed = TRUE,  # Data schema box is initially collapsed
                                 dataTableOutput(ns("schema"))
                             )
                      )
                    )
           ),
           tabPanel("Data",
                    fluidRow(
                      column(12,
                             box(title = "Raw Data",
                                 status = "white",
                                 solidHeader = TRUE,
                                 collapsible = TRUE,
                                 elevation = 3,
                                 width = 12,
                                 collapsed = FALSE,  # Raw Data box is initially expanded
                                 dataTableOutput(ns("rawtable"))),
                             box(title = "Summary statistics",
                                 status = "white",
                                 solidHeader = TRUE,
                                 collapsible = TRUE,
                                 elevation = 3,
                                 width = 12,
                                 collapsed = TRUE,  # Summary statistics box is initially collapsed
                                 verbatimTextOutput(ns("summary")))
                      )
                    )
           ),
           tabPanel("Plot",
                    fluidRow(
                      column(3,
                             box(title = "Select Plot Type",
                                 status = "white",
                                 solidHeader = TRUE,
                                 collapsible = FALSE,  # Plot Type box is not collapsible
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
                                 collapsible = FALSE,  # Select Variables box is not collapsible
                                 elevation = 3,
                                 width = 12,
                                 selectInput("Xvar",
                                             "X",
                                             choices = ""),
                                 selectInput("Yvar",
                                             "Y",
                                             choices ="")
                             )
                      )
                    ),
                    column(9,
                           box(title = "Plot Area",
                               status = "white",
                               solidHeader = TRUE,
                               collapsible = FALSE,  # Plot Area box is not collapsible
                               elevation = 3,
                               width = 12,
                               plotOutput(ns("plot")
                               )
                           )
                    )
           )
    )
  )
}

# Define a custom server function for a tab item
tabItemServer <- function(id, exampleMeta) {
  moduleServer(
    id,
    function(input, output, session) {

      # Define a function to create metadata
      CreateMetaData <- function(nameD, Yeard, DoU) {
        if (DoU == FALSE){
          filename <- reactive({
            list.files("data", pattern = paste0(nameD, exampleMeta, Yeard))
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


      # Render and output raw data and summary

      ## Input and read data

      data <- reactive({
        req(exampleMeta)
        path <- file.path("data",paste0("cow", exampleMeta, "Cant1997.csv"))

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

      # Output plot
      ## function to observe input data and update input selection for X and Y
      output$plot <- renderPlot({
        ggplot(data(),
               aes_string(input$Xvar,input$Xvar))+
          geom_boxplot()
      })



    }


  )
}
