library(shiny)
library(tidyverse)
library(cowplot)

shinyServer(function(input, output) {
        
        color_scheme = c("#000000", "#E69F00", "#56B4E9") #, "#CC79A7", "#D55E00", "#009E73")
    
        data <- reactive({
            read.delim("data.tab", header=TRUE) %>%
                filter(Supervisor %in% input$supervisor) %>%
                filter(Event %in% input$event) %>%
                mutate(EventDate = as.Date(EventDate, format = "%d.%m.%y")) %>%
                mutate(StartDate = as.Date(StartDate, format = "%d.%m.%y")) %>%
                mutate(days = EventDate - StartDate) %>%
                mutate(weeks = days/7)
                })
        
        output$table = renderTable(data())
        
        output$plot = renderPlot({
               
                ### order data_plot
                if (input$sort_crit == "Name") {data_plot = data()[order(data()$Name, decreasing = input$sort_dir), ]}
                if (input$sort_crit == "StartDate") {data_plot = data()[order(data()$StartDate, decreasing = input$sort_dir), ]}
                    
                n = length(unique(data_plot$Name))
    
                ### plot with relative or absolute time
                if (input$time == "relative") {
                        plot = ggplot(data_plot, aes(x = weeks, y = factor(Name, levels = unique(Name)), color = Event)) +
                                geom_line(size = 0.25, color = "black") +
                                geom_point(size = 3) +
                                ylab(paste("Students n =", n)) +
                                xlab("weeks") 
                                # scale_color_manual(values = color_scheme,
                                #                  name = "Event",
                                #                  breaks = c("Committee Meeting", "Progress Meeting", "Appraisal Meeting"),
                                #                  labels = c("Committee Meeting", "Progress Meeting", "Appraisal Meeting"))
                } else {
                        plot = ggplot(data_plot, aes(x = EventDate, y = factor(Name, levels = unique(Name)), color = Event)) +
                                geom_line(size = 0.25, color = "black") +
                                geom_point(size = 3) +
                                ylab(paste("Students n =", n)) +
                                xlab("date") 
                                # scale_color_manual(values = color_scheme,
                                #            name = "Event",
                                #            breaks = c("Committee Meeting", "Progress Meeting", "Appraisal Meeting"),
                                #            labels = c("Committee Meeting", "Progress Meeting", "Appraisal Meeting"))
                  
                        }
                print(plot)
        }) #, height = 700)
}
)
