shiny::shinyApp(
  ui = shiny::fluidPage(
    shiny::fluidRow(
      shiny::plotOutput('categDist', height = "400px")
    )
  ),

  server = function(input, output, session) {
    sampled_data <- shiny::reactivePoll(intervalMillis = 2000,
                                        session = NULL,
                                        checkFunc = function() return(sample(1:100, 1)),
                                        valueFunc = function() return(dataset[sample(1:nrow(dataset), 100),]))

    output$categDist <- renderPlot(height = 400, {
      sampled_data <- sampled_data()
      shinyPipeline %>>% setInput(input = sampled_data) -> shinyPipeline
      shinyPipeline %>>% generateOutput %>>% getOutputById("1")
    })
  },

  options = list(height = 500)
)
