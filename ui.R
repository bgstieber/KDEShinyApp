library(shiny)

shinyUI(fluidPage(
  
    titlePanel("Bandwidth Selection Demonstration"),
  fluidRow(
      #left column
      column(2,
      #data selector         
      radioButtons('data', 'Data',
                   choices = c('Normal' = 'n1',
                               'Mixture of Two Normals' = 'n2',
                               'Equal Mixture of Three Gammas' = 'g1',
                               'Old Faithful Waiting Times' = 'old',
                               'Top 250 Beers' = 'beer',
                               'PGA Tour Driving Distance' = 'golf',
                               'Your File Upload' = 'upload')),
      #data descriptions for old faithful, beer, and golf
      #old faithful
      conditionalPanel("input.data == 'old'",
                       helpText('Old Faithful waiting times from',
                                'Azzalini and Bowman (1990). Data',
                                'documents waiting times (in minutes)',
                                'for an eruption at Old Faithful geyser',
                                'in Yellowstone National Park (WY).',
                                'Data collected from 8/1-8/15/1985 (N = 299)')),
      
      #beer
      conditionalPanel("input.data == 'beer'",
                       helpText('Alcohol by volume ratings (%) of top',
                                '250 beers as rated on BeerAdvocate.com. (N = 248)')),
      
      #golf
      conditionalPanel("input.data == 'golf'",
                       helpText('PGA Tour driving distance statistics',
                                'for 1986, 1996, and 2015 seasons.',
                                'Note the bimodality of the distribution,',
                                'as the 2015 season marks a significant departure from',
                                'the technology of the 20th century. The titanium',
                                'driver and improved golf ball technology have',
                                'created many "bombers" off the tee. (N = 552)')),
      
      
      #size of simulation (if applicable)
      conditionalPanel("(input.data == 'n1' || input.data == 'n2' || input.data == 'g1')",
                       sliderInput("N",
                                   "N:",
                                   min = 10,
                                   max = 5000,
                                   value = 500,
                                   step = 250)
      ),
      
      #control normal
      
      conditionalPanel("input.data == 'n1'",
                       numericInput('mean1', 'Mean for Normal',
                                    value = 0)),
      
      
      conditionalPanel("input.data == 'n1'",
                       numericInput('sd1', 'SD for Normal',
                                    value = 1, min = 0)),
      
      #control mixture of normals
      conditionalPanel("input.data == 'n2'",
                       h4('Parameters for First Normal'),
                       sliderInput('pi1.m1', 'Mixing Weight for 1st Normal',
                                    value = 0.5,
                                   min = 0, max = 1)
                       ),
      
      conditionalPanel("input.data == 'n2'",
                       fluidRow(
                        column(6,
                       numericInput('mean.m1', 'Mean',
                                    value = 0)),
                        column(6,
                               numericInput('sd.m1', 'SD',
                                            value = 1, min = 0)
                               )
                       )),
      
     
      conditionalPanel("input.data == 'n2'",
                       h4('Parameters for Second Normal'),
                       fluidRow(
                           column(6,
                                  numericInput('mean.m2', 'Mean',
                                               value = 4)),
                           column(6,
                                  numericInput('sd.m2', 'SD',
                                               value = 2, min = 0)
                           )
                       )),
      
      #control mixture of gammas
      
      conditionalPanel("input.data == 'g1'",
                       h4('Parameters for First Gamma'),
                       fluidRow(
                           column(6,
                       numericInput('shape.g1', 'Shape',
                                    value = 1)),
                           column(6,
                       numericInput('scale.g1', 'Scale',
                                    value = 20)))),
      
      conditionalPanel("input.data == 'g1'",
                       h4('Parameters for Second Gamma'),
                       fluidRow(
                           column(6,
                                  numericInput('shape.g2', 'Shape',
                                               value = 4)),
                           column(6,
                                  numericInput('scale.g2', 'Scale',
                                               value = 5)))),
      conditionalPanel("input.data == 'g1'",
                       h4('Parameters for Third Gamma'),
                       fluidRow(
                           column(6,
                                  numericInput('shape.g3', 'Shape',
                                               value = 20)),
                           column(6,
                                  numericInput('scale.g3', 'Scale',
                                               value = 1)))),
      
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
      br(),
      helpText(a(href="https://github.com/bgstieber/KDEShinyApp", 
                 target="_blank", "View Shiny code"))
      ),
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
