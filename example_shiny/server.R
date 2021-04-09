library(tidyverse)
d <- read_rds("data/ON_mortality.RDS")

# Define a server for the Shiny app
function(input, output) {
  
  # Fill in the spot we created for a plot
  output$hazardPlot <- renderPlot({
    
    p <- d %>% 
      mutate(age = as.numeric(age)) %>% 
      filter(year==input$year) %>% 
      ggplot(aes(age, log(hx))) + 
      geom_line(aes(color = "data")) + 
      #scale_y_log10() + 
      theme_bw() + 
      ylab("(log) hazard rate") + 
      ggtitle(paste0("Hazard of dying by age, Ontario ", input$year )) + 
      scale_color_manual(name = "", values = c("data" = "black", "fit" = "red")) +
      ylim(c(-11,0))
    
      if(input$addGompertz==FALSE){
        p
      }
    
      else if (input$addGompertz== TRUE){
        
        dd <- d %>% 
          mutate(age = as.numeric(age)) %>% 
          filter(age>39) %>% 
          filter(year==input$year)
        
        mod <- lm(log(hx)~age, data = dd)
        b <- coef(mod)[2]
        a <- coef(mod)[1]
        mode_age = 1/b*log(b/exp(a))
        
        p + 
          geom_abline(aes(slope = b, intercept = a, color = "fit") , lwd = 1.1) +
          annotate("text", 
                   label = paste("Estimated modal age of death:", round(mode_age)),
                   col = 2,
                   x = 30, y = -2)
          
      }
    
  })
}