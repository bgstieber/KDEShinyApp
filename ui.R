library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Bandwidth Selection Demonstration"),
  

  sidebarLayout(
    sidebarPanel(
      
      radioButtons('data', 'Data',
                   choices = c('N(0,1)' = 'n1',
                               '0.5 N(0,1) + 0.5 N(4,4)' = 'n2',
                               'Old Faithful' = 'old',
                               'Beer ABV' = 'beer',
                               'PGA Tour Driving Distance (1986, 1996, 2015)' = 'golf',
                               'Upload' = 'upload')),
      conditionalPanel("input.data == 'upload'",
                       fileInput('file1', 'Choose file to upload',
                                 accept = c(
                                     'text/csv',
                                     'text/comma-separated-values',
                                     'text/tab-separated-values',
                                     'text/plain',
                                     '.csv',
                                     '.tsv'
                                 )
                       )
                      ),
      
      conditionalPanel("input.data == 'upload'",
                       helpText("Upload a file of your choosing.",
                                "This app will only look in the",
                                "first column of your .csv!")),
      
      conditionalPanel("input.data == 'upload'",
                       checkboxInput('header', 'Header', TRUE)
      ),
      conditionalPanel("input.data == 'upload'",
                       radioButtons('sep', 'Separator',
                                    c(Comma=',',
                                      Semicolon=';',
                                      Tab='\t'),
                                    ',')
      ),
      conditionalPanel("input.data == 'upload'",
                       radioButtons('quote', 'Quote',
                                    c(None='',
                                      'Double Quote'='"',
                                      'Single Quote'="'"),
                                    '"')
      ),
      
      conditionalPanel("(input.data == 'n1' || input.data == 'n2')",
      sliderInput("N",
                  "N:",
                  min = 100,
                  max = 1000,
                  value = 300)
      ),
      
      sliderInput("bins",
                  "Histogram bins:",
                  min = 5,
                  max = 75,
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
    

    mainPanel(
      plotOutput("distPlot")
    )
  )
))
