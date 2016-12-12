library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Old Faithful Waiting Times KDE"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Number of bins:",
                  min = 5,
                  max = 50,
                  value = 25),
      radioButtons('dens',
                   'Plot Density',
                   c(FALSE, TRUE)
                   ),
      conditionalPanel(condition = "input.dens",
                       numericInput('bw','Density Bandwidth',1, min = 1e-5)),
      conditionalPanel(condition = "input.dens",
                       checkboxGroupInput('bws','Bandwidth Selectors',
                                          choices = c('Sheather-Jones' = 'SJ',
                                                      'Silverman ROT' = 'nrd0',
                                                      'Unbiased CV' = 'ucv',
                                                      'Maximal Smoothing' = 'MS')))
      ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
))