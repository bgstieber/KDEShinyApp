#load packages
library(shiny)
library(rvest)
library(RCurl)
library(MASS)
#grab beer data from github
git_file <- 
    getURL("https://raw.githubusercontent.com/bgstieber/Top250Beer/master/Top250Beers.csv")
beer_data <- read.csv(text = git_file, stringsAsFactors = FALSE)
abv.complete <- beer_data[!is.na(beer_data$ABV), ]$ABV
#grab pga tour data 
pga1986 <- read_html('http://www.pgatour.com/stats/stat.101.1986.html')
pga1996 <- read_html('http://www.pgatour.com/stats/stat.101.1996.html')
pga2006 <- read_html('http://www.pgatour.com/stats/stat.101.2006.html')
pga2015 <- read_html('http://www.pgatour.com/stats/stat.101.2015.html')

dd1986 <- pga1986 %>% 
    html_nodes('table') %>%
    .[2] %>%
    html_table(., header = TRUE)

dd1996 <- pga1996 %>% 
    html_nodes('table') %>%
    .[2] %>%
    html_table(., header = TRUE)

dd2006 <- pga2006 %>% 
    html_nodes('table') %>%
    .[2] %>%
    html_table(., header = TRUE)

dd2015 <- pga2015 %>%
    html_nodes('table') %>%
    .[2] %>%
    html_table(., header = TRUE)

dd <- c(dd1986[[1]]$AVG., dd1996[[1]]$AVG., dd2006[[1]]$AVG., dd2015[[1]]$AVG.)
dd.avg <- c(mean(dd1986[[1]]$AVG.), 
            mean(dd1996[[1]]$AVG.), 
            mean(dd2006[[1]]$AVG.),
            mean(dd2015[[1]]$AVG.))

#approximate variance function

app_var <- function(x, bw, alpha){
    R <- 1 / (2 * sqrt(pi)) # assume gaussian
    h <- bw(x)
    n <- length(x)
    range_x <- range(x)
    range_x <- range_x + c(-.5, .5) #wiggle room
    dens_x <- density(x, from = range_x[1], to = range_x[2],
                      kernel = 'gaussian', bw = h, n = 2048)
    
    var.dens <- (1 / (n * h)) * R * dens_x$y - ((1 / n) * (dens_x$y) ^ 2)
    
    lower <- dens_x$y - (qnorm(1 - (alpha / 2)) * sqrt(var.dens))
    upper <- dens_x$y + (qnorm(1 - (alpha / 2)) * sqrt(var.dens))
    
    data.frame(grid = dens_x$x,
               yhat = dens_x$y,
               lower = lower,
               upper = upper)
}

#boostrap percentile function
boot_dens <- function(x, B = 1000, bw, alpha){
    
    range_x <- range(x)
    range_x <- range_x + c(-.5, .5) #wiggle room
    
    orig_dens <- density(x, from = range_x[1], to = range_x[2], bw = bw(x))
    
    boot_dens <- matrix(0, ncol = B, nrow = length(orig_dens$x))
    
    for(i in 1:B){
        boot.x <- sample(x, length(x), replace = T)
        boot.density <- density(boot.x, from = range_x[1], to = range_x[2], 
                                bw = bw(boot.x))
        boot_dens[,i] <- boot.density$y
    }
    
    lower <- apply(boot_dens, 1, function(mat) quantile(mat, alpha / 2))
    upper <- apply(boot_dens, 1, function(mat) quantile(mat, 1 - (alpha / 2)))
    
    data.frame(grid = orig_dens$x,
               yhat = orig_dens$y,
               lower = lower,
               upper = upper)
    
}

#maximal smoothing function
bw.MS <- function(x){ 3 * ((2 * sqrt(pi))^(-1) / (35 * length(x)))^(1/5) * sd(x) }


shinyServer(function(input, output) {
#user file upload  
    inFile <- reactive({input$file1})
#reactive element for a file upload
    data.upload <- reactive({
        if(is.null(inFile())){
            NULL
        }else{
            read.csv(inFile()$datapath, header = input$header,
                     sep = input$sep, quote = input$quote)[,1]
        }
    })
 #create list of data, select matching list  
  mydata <- reactive({list(
    'n1' = rnorm(input$N, mean = input$mean1, sd = input$sd1),
    'n2' = c(rnorm(n = input$N * input$pi1.m1, 
                   mean = input$mean.m1, sd = input$sd.m1), 
             rnorm(n = input$N * (1 - input$pi1.m1),
                   mean = input$mean.m2, sd = input$sd.m2)),
    'g1' = c(rgamma(n = input$N / 3, 
                    shape = input$shape.g1, scale = input$scale.g1),
             rgamma(n = input$N / 3, 
                    shape = input$shape.g2, scale = input$scale.g2),
             rgamma(n = input$N / 3, 
                    shape = input$shape.g3, scale = input$scale.g3)),
    'old' = geyser[,2],
    'beer' = abv.complete,
    'golf' = dd,
    'upload' = c(data.upload()) #omit missing values
  )[[input$data]]})
  
#reverse list of bandwidths
  rev_h <- c('SJ' = 'Sheather-Jones',
             'MS' = 'Maximal Smoothing',
             'nrd0' = 'Silverman ROT',
             'ucv' = 'Unbiased CV')
#x axis labels
  x.lab <- reactive({switch(input$data,
                 'n1' = paste0('N(', input$mean1, ", ", round(input$sd1^2,2),')'),
                 'n2' = paste0(round(input$pi1.m1, 2), ' N(', input$mean.m1,
                               ', ', round(input$sd.m1^2, 2), ') + ',
                               round(1-input$pi1.m1, 2), ' N(', input$mean.m2, 
                               ', ', round(input$sd.m2^2, 2), ')'),
                 'g1' = paste0('1/3', 
                               ' G(', input$shape.g1, ', ', input$scale.g1, ')',
                               ' + ', '1/3',
                               ' G(', input$shape.g2, ', ', input$scale.g2, ')',
                               ' + ', '1/3',
                               ' G(', input$shape.g3, ', ', input$scale.g3, ')'
                 ),
                 'old' = 'Old Faithful Waiting Times (minutes)',
                 'beer' = 'ABV for Top 250 Beers (as rated on BeerAdvocate) (%)',
                 'golf' = 'PGA Tour Driving Distance (1986, 1996, 2006, 2015) (yards)',
                 'upload' = 'Your Data')})
#legend position 
  leg.pos <- reactive({switch(input$data,
                            'n1' = 'topleft',
                            'n2' = 'topright',
                            'g1' = 'topright',
                            'old' = 'topleft',
                            'beer' = 'topright',
                            'golf' = 'topright',
                            'upload' = 'topleft')})
  
  
#rended the plot
  output$Histogram <- renderPlot({

    x <- mydata()
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    h.MS <- bw.MS(x)
    #colors based on brewer palette set1
    my_fav_colors <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3")
    
    #draw histogram
    if(input$dens){
    hist(x, breaks = bins, col = 'grey93',
         probability = TRUE,
         xlab = x.lab(),
         main = ifelse(input$data == 'golf', 
                       'Vertical lines indicate average distance for\n1986 (orange), 1996 (blue), 2006 (green), and 2015 (black)',
                       ''),
         ylim = c(0, 1.025 * max(hist(x, breaks = bins)$density)))
        if(input$data == 'golf'){
            abline(v = dd.avg, col = c('darkorange2','dodgerblue2', 'seagreen','black'), lwd = 3,
                   lty = 2)
        }
        
    }else{
        hist(x, breaks = bins, col = 'grey93',
             probability = FALSE,
             xlab = x.lab(),
             main = ifelse(input$data == 'golf', 
                           'Vertical lines indicate average distance for\n1986 (orange), 1996 (blue), 2006 (green), and 2015 (black)',
                           ''))
        if(input$data == 'golf'){
            abline(v = dd.avg, col = c('darkorange2','dodgerblue2', 'seagreen','black'), lwd = 3,
                   lty = 2)
        }
    }
    
    box() #put a box around it
    
    if(input$dens){
        
        if(input$CI){
            
            #which bandwidth to use
            bw.CI <- switch(input$CI_h,
                            'SJ' = bw.SJ,
                            'nrd0' = bw.nrd0,
                            'ucv' = bw.ucv,
                            'MS' = bw.MS)
            
            if(input$citype == 'approx'){
                #calculate interval
                ci.data <- app_var(x = x, bw = bw.CI, alpha = input$alph)
                #plot density(x)
                lines(ci.data$grid, ci.data$yhat, col = '#b70101', lwd = 2)
                #plot CI ribbon
                polygon(c(ci.data$grid, rev(ci.data$grid)),
                        c(ci.data$upper, rev(ci.data$lower)),
                        col = adjustcolor('grey50', .3))
                
                #add legend
                legend(leg.pos(), 
                       legend = c(paste(rev_h[names(rev_h) == input$CI_h], '-',
                                        round(bw.CI(x), 2)),
                                  'Percentile Interval'),
                       col = c('#b70101', 'grey'), lwd = c(2, NA), lty = c(1, NA),
                       pch = c(NA, 15), pt.cex = 2)
                
            }else if(input$citype == 'boot'){
                #calculate interval
                ci.data <- boot_dens(x = x, B = input$boots, bw = bw.CI, alpha = input$alph)
                #plot density(x)
                lines(ci.data$grid, ci.data$yhat, col = '#b70101', lwd = 2)
                #plot CI ribbon
                polygon(c(ci.data$grid, rev(ci.data$grid)),
                        c(ci.data$upper, rev(ci.data$lower)),
                        col = adjustcolor('grey50', .3))
                
                #add legend
                legend(leg.pos(), 
                       legend = c(paste(rev_h[names(rev_h) == input$CI_h], '-',
                                        round(bw.CI(x), 2)),
                                  'Percentile Interval'),
                       col = c('#b70101', 'grey'), lwd = c(2, NA), lty = c(1, NA),
                       pch = c(NA, 15), pt.cex = 2)
            }
        }else{    
        
            lines(density(x, bw = input$bw))
      
            if(length(input$bws) > 0){
        
                lapply(
                    input$bws, function(y){
                        if(y == 'MS'){
                            lines(density(x, bw = h.MS), 
                                  lwd = 2, col = my_fav_colors[which(y == input$bws)])
                        }else{
                            lines(density(x, bw = y), 
                                  lwd = 2, col = my_fav_colors[which(y == input$bws)])
                        }
                    }
                )
        
                lgd_text <- 
                    lapply(input$bws, function(y) 
                        if(y == 'MS'){
                            paste(rev_h[names(rev_h) == y], '-', 
                                  round(h.MS, 2))
                        }else{
                            paste(rev_h[names(rev_h) == y], '-', 
                                  round(density(x, bw = y)$bw, 2))
                        }
                    )
                legend(leg.pos(), legend = lgd_text, col = my_fav_colors[1:length(input$bws)],
                       lty = 1, lwd = 2)
            }
        }
    }
  })
})
