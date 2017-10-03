library(shiny)

library(tidyr)
#library(cowplot)
library(dplyr)
library(ggplot2)

library(tidyverse)

shinyServer(function(input, output) {
        
        table = data.frame("col1" = c(1, 2, 3), "col2" = c(4, 5, 3)) %>%
                mutate(col1 = col1*2)
        
        output$plot = renderPlot({
                ggplot(table, aes(x = col1, y = col2)) +
                        geom_point()
                })

        

})

