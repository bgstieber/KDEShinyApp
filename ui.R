library(shiny)

shinyUI(fluidPage(
  
    titlePanel("Bandwidth Selection Demonstration"),
  fluidRow(
      #left column
      column(2,
      #data selector         
      radioButtons('data', 'Data',
                   choices = c('N(0,1)' = 'n1',
                               '1/2 N(0,1) + 1/2 N(4,4)' = 'n2',
                               '1/3 G(1,20) + 1/3 G(4,5) + 1/3 G(20,1)' = 'g1',
                               'Old Faithful Waiting Times' = 'old',
                               'Top 250 Beers' = 'beer',
                               'PGA Tour Driving Distance (1986, 1996, 2015)' = 'golf',
                               'Your File Upload' = 'upload')),
      #file upload panel
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
      #file upload text
      conditionalPanel("input.data == 'upload'",
                       helpText("Upload a file of your choosing.",
                                "This app will only look in the",
                                "first column of your .csv!")),
      #file upload options
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
      #size of simulation (if applicable)
      conditionalPanel("(input.data == 'n1' || input.data == 'n2' || input.data == 'g1')",
      sliderInput("N",
                  "N:",
                  min = 100,
                  max = 1000,
                  value = 300)
      )),
      #middle column-plot
      column(8,
             plotOutput("Histogram", height = "750px")
      ),
      #right column
      column(2,
      #histogram bins
      sliderInput("bins",
                  "Histogram bins:",
                  min = 5,
                  max = 75,
                  value = 25),
      #plot type
      radioButtons('dens',
                   'Plot Type',
                   c('Frequency' = FALSE, 'Density' = TRUE)
                   ),
      #control smoothing
      conditionalPanel(condition = "input.dens == 'TRUE' & input.CI == 'FALSE'",
                       numericInput('bw','Density Bandwidth', 1, min = 1e-3)),
      #bandwidth selector
      conditionalPanel(condition = "input.dens == 'TRUE' & input.CI == 'FALSE'",
                       checkboxGroupInput('bws','Bandwidth Selectors',
                                          choices = c('Sheather-Jones' = 'SJ',
                                                      'Silverman ROT' = 'nrd0',
                                                      'Unbiased CV' = 'ucv',
                                                      'Maximal Smoothing' = 'MS'))),
      #draw intervals instead?
      conditionalPanel("input.dens == 'TRUE'",
                       radioButtons('CI',
                                    'Plot Pointwise Confidence Interval',
                                    c('Yes' = TRUE, 'No' = FALSE),
                                    FALSE)),
      #what type of intervals?
      conditionalPanel("input.CI == 'TRUE' & input.dens == 'TRUE'",
                       radioButtons('citype',
                                    'Pointwise Interval Computation',
                                    c('Bootstrap Percentile' = 'boot',
                                      'Approximate Variance' = 'approx'),
                                    'approx')),
      #select bandwidth for intervals
      conditionalPanel("input.CI == 'TRUE' & input.dens == 'TRUE'",
                       radioButtons('CI_h',
                                    'Confidence Interval Bandwidth',
                                    choices = c('Sheather-Jones' = 'SJ',
                                                'Silverman ROT' = 'nrd0',
                                                'Unbiased CV' = 'ucv',
                                                'Maximal Smoothing' = 'MS'),
                                    'MS')),
      #control pointwise alpha
      conditionalPanel("input.CI == 'TRUE' & input.dens == 'TRUE'",
                       sliderInput('alph',  HTML("&alpha;"), min = 0.01, max = 0.50,
                                   value = .05, step = .01)
                       
                       ),
      #control bootstrap iters
      conditionalPanel("input.CI == 'TRUE' & input.dens == 'TRUE' & input.citype == 'boot'",
                       numericInput('boots',  "Bootstrap Iterations", 
                                   min = 500, max = 10000,
                                   value = 1000, step = 100)
                       
      ),
      #note about boostrap computation
      conditionalPanel("input.CI == 'TRUE' & input.dens == 'TRUE' & input.citype == 'boot'",
                       helpText('Bootstrap intervals are computationally intensive,',
                                'so it may take a few seconds for the plot to update.')
                       
      )

                       
      )
      )
     )
)
