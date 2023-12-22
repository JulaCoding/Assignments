library(shiny)
library(ggplot2)

shinyUI(fluidPage(
     titlePanel("Interactive Iris Explorer"),
     sidebarLayout(
          sidebarPanel(
               selectInput("x_var", "X-axis Variable", choices = names(iris)),
               selectInput("y_var", "Y-axis Variable", choices = names(iris)),
               sliderInput("n_obs", "Number of Observations", min = 1, max = nrow(iris), value = 50),
               plotOutput("scatter_plot")
          ),
          mainPanel(
               verbatimTextOutput("summary_text")
          )
     ))
)
     