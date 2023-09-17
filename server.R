library(shiny)

shinyServer(function(session, input, output) {
  source(here("Analysis", "main_task", "plotting_funcs.R"), T)
  # input$separate_by_exposure <- as.logical(input$separate_by_exposure)
  
  
  # input$separate_by_exposure <- reactive({
  #   as.logical(input$separate_by_exposure)
  # })
  
  observe({
    # medium <<- "shiny"
    output$choice_curve_plot <- renderPlot({
      print(pryr::mem_used())
      choice_and_learning_plots("choice_curve")
    })
    
    output$learning_curve_plot <- renderPlot({
      # myplot <- choice_and_learning_plots("learning_curve")
      # ggsave("plot.png", plot = myplot, width = 10, height = 7)
      print(pryr::mem_used())
      choice_and_learning_plots("learning_curve")
    })
    
    output$fits_plot <- renderPlot({
      print(pryr::mem_used())
      fits_plot("meaningless filler text")
    })
    
    
    
  })
})
