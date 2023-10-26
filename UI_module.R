uiItem_UI <- function(id) {
  ns <- NS(id)
  tagList(
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
                    MetaUI(ns("meta"))),
           tabPanel("Data",
                    DataUI(ns("data"))),
           tabPanel("Plot",
                    PlotUI(ns("plot")))

    )

  )
}

uiItem_Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      resultMeta <- MetaServer("meta")
      resultData <-DataServer("data")

      reactive(resultMeta())

    }
  )
}
