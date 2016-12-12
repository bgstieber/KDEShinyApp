library(shiny)
library(rvest)
library(RCurl)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
    
  mydata <- reactive({list(
    'n1' = rnorm(input$N),
    'n2' = c(rnorm(n = input$N / 2), 
             rnorm(n = input$N / 2, mean = 4, sd = 2)),
    'old' = faithful[,2],
    'cars' = mtcars[,1]
  )[[input$data]]})
  
  rev_h <- c('SJ' = 'Sheather-Jones',
             'MS' = 'Maximal Smoothing',
             'nrd0' = 'Silverman ROT',
             'ucv' = 'Unbiased CV')
  
  # Expression that generates a histogram. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should re-execute automatically
  #     when inputs change
  #  2) Its output type is a plot
  output$distPlot <- renderPlot({
 # Old Faithful Geyser data
    #x    <- mydata[[input$data]]
    x <- mydata()
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    h.MS <- 3 * ((2 * sqrt(pi))^(-1) / (35 * length(x)))^(1/5) * sd(x)
    
    my_fav_colors <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3")
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'grey93',
         probability = input$dens)
    
    if(input$dens){
      lines(density(x, bw = input$bw))
      
      if(length(input$bws) > 0){
        
      lapply(input$bws, function(y){
        if(y == 'MS'){
          lines(density(x, bw = h.MS), lwd = 2, col = my_fav_colors[which(y == input$bws)])
        }else{
          lines(density(x, bw = y), lwd = 2, col = my_fav_colors[which(y == input$bws)])
        }
      })
        
        lgd_text <- lapply(input$bws, function(y) 
          if(y == 'MS'){
          paste(rev_h[names(rev_h) == y], '-', round(h.MS, 2))
          }else{
            paste(rev_h[names(rev_h) == y], '-', round(density(x, bw = y)$bw, 2))
          }
        )
        legend('topleft', legend = lgd_text, col = my_fav_colors[1:length(input$bws)],
               lty = 1, lwd = 2)
      }
    }
  })
})
