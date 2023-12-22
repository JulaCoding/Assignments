library(shiny)
library(ggplot2)

shinyServer(function(input, output) {
     output$scatter_plot <- renderPlot({
          sampled_data <- iris[sample(nrow(iris), input$n_obs), ]
          
          ggplot(sampled_data, aes_string(x = input$x_var, y = input$y_var)) +
               geom_point() +
               labs(title = "Interactive Scatter Plot", x = input$x_var, y = input$y_var)
     })
     
     output$summary_text <- renderText({
          summary_data <- summary(iris[, c(input$x_var, input$y_var)])
          paste("Summary Statistics:\n", capture.output(summary_data))
     })
})