library(shiny)
library(rvest)
library(RCurl)

git_file <- 
    getURL("https://raw.githubusercontent.com/bgstieber/Top250Beer/master/Top250Beers.csv")
beer_data <- read.csv(text = git_file, stringsAsFactors = FALSE)
abv.complete <- beer_data[!is.na(beer_data$ABV), ]$ABV

pga1986 <- read_html('http://www.pgatour.com/stats/stat.101.1986.html')
pga1996 <- read_html('http://www.pgatour.com/stats/stat.101.1996.html')
pga2015 <- read_html('http://www.pgatour.com/stats/stat.101.2015.html')

dd1986 <- pga1986 %>% 
    html_nodes('table') %>%
    .[2] %>%
    html_table(., header = TRUE)

dd1996 <- pga1996 %>% 
    html_nodes('table') %>%
    .[2] %>%
    html_table(., header = TRUE)

dd2015 <- pga2015 %>%
    html_nodes('table') %>%
    .[2] %>%
    html_table(., header = TRUE)

dd <- c(dd1986[[1]]$AVG., dd1996[[1]]$AVG., dd2015[[1]]$AVG.)

shinyServer(function(input, output) {
  
    inFile <- reactive({input$file1})
    
    data.upload <- reactive({
        if(is.null(inFile())){
            NULL
        }else{
            read.csv(inFile()$datapath, header = input$header,
                     sep = input$sep, quote = input$quote)[,1]
        }
    })
    
  mydata <- reactive({list(
    'n1' = rnorm(input$N),
    'n2' = c(rnorm(n = input$N / 2), 
             rnorm(n = input$N / 2, mean = 4, sd = 2)),
    'g1' = c(rgamma(n = input$N / 3, shape=1, scale=20),
             rgamma(n = input$N / 3, shape = 4, scale = 5),
             rgamma(n = input$N / 3, shape = 20, scale = 1)),
    'old' = faithful[,2],
    'beer' = abv.complete,
    'golf' = dd,
    'upload' = c(data.upload())
  )[[input$data]]})
  
  rev_h <- c('SJ' = 'Sheather-Jones',
             'MS' = 'Maximal Smoothing',
             'nrd0' = 'Silverman ROT',
             'ucv' = 'Unbiased CV')
  
  x.lab <- reactive({switch(input$data,
                 'n1' = 'N(0,1)',
                 'n2' = '0.5 N(0,1) + 0.5 N(4, 4)',
                 'g1' = '1/3 G(1,20) + 1/3 G(4,5) + 1/3 G(20,1)',
                 'old' = 'Old Faithful Waiting Times (minutes)',
                 'beer' = 'ABV for Top 250 Beers (as rated on BeerAdvocate) (%)',
                 'golf' = 'PGA Tour Driving Distance (1986, 1996, 2015) (yards)',
                 'upload' = 'Your Data')})
  
  leg.pos <- reactive({switch(input$data,
                            'n1' = 'topleft',
                            'n2' = 'topright',
                            'g1' = 'topright',
                            'old' = 'topleft',
                            'beer' = 'topright',
                            'golf' = 'topright',
                            'upload' = 'topleft')})
  
  

  output$distPlot <- renderPlot({

    x <- mydata()
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    h.MS <- 3 * ((2 * sqrt(pi))^(-1) / (35 * length(x)))^(1/5) * sd(x)
    
    my_fav_colors <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3")
    
    
    hist(x, breaks = bins, col = 'grey93',
         probability = input$dens,
         xlab = x.lab(),
         main = '')
    
    box()
    
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
        legend(leg.pos(), legend = lgd_text, col = my_fav_colors[1:length(input$bws)],
               lty = 1, lwd = 2)
      }
    }
  })
})
