library(shiny)
#library(tidyverse)
library(tidyr)
library(dplyr)
library(readr)
library(ggplot2)
library(cowplot)

shinyServer(function(input, output) {
        
        data <- reactive({
                ### data import
                data = read.delim("data.tab", header=FALSE)[,1:7]
                colnames(data) = c("id", "tpx_date_kidney", "tpx_date_lung", "death_dt", "vis_type", "vis_date", "organ_type")
                deaths = data %>%
                        filter(death_dt != "") %>%
                        select(id, death_dt, organ_type, tpx_date_kidney, tpx_date_lung) %>%
                        unique() %>%
                        mutate(vis_date = death_dt) %>%
                        mutate(vis_type = "Death")
                data = rbind(data, deaths) %>%
                        filter(vis_type != "") %>%
                        mutate(tpx_date = paste0(tpx_date_lung, tpx_date_kidney)) %>%
                        mutate(tpx_date = as.Date(tpx_date, format = "%Y-%m-%d")) %>%
                        mutate(vis_date = as.Date(vis_date, format = "%Y-%m-%d")) %>%
                        mutate(days = vis_date - tpx_date) %>%
                        mutate(weeks = days/7)
                return(data)
                })
        
        ids_routine <- reactive ({
                data() %>% filter(vis_type == "Routine Transplantation") %>% select(id) %>% unique()
                })
        
        kidney_pairs_ids = c("aag951", "bgk952", "drr356", "fed741", "flr704", "hmf012", "hyd494",
                             "ihf319", "iwv346", "jns976", "kht322", "knb739", "mek642", "pip445", "poo581",
                             "pqg516", "qfv506", "qnx429", "qyg532", "sjp926", "tqm938", "tvy653", "twn073",
                             "ume111", "utf849", "vnf260", "vpi912", "wdk036", "whf951", "xbq419", "xph346", "ypg234")
                
        data_filtered <- reactive({
                ### filter for organs
                data_filtered = data() %>% filter(organ_type %in% input$organs)
                ### filter for ids
                if (input$ids == "Kidney Pairs") {data_filtered = data_filtered %>% filter(id %in% kidney_pairs_ids)}
                return(data_filtered)
                })
        
        output$plot = renderPlot({
                ### filter for scenarios
                if (input$scenario[1] == "symptomatic") {
                        data_plot = data_filtered() %>% filter(!(id %in% ids_routine()$id))
                        color_scheme = c("#000000", "#D55E00", "#009E73")
                } else if (length(input$scenario) == 2) {
                        data_plot = data_filtered()
                        color_scheme = c("#000000", "#E69F00", "#56B4E9", "#CC79A7", "#D55E00", "#009E73")
                } else {
                        data_plot = data_filtered() %>% filter(id %in% ids_routine()$id)
                        color_scheme = c("#000000", "#E69F00", "#56B4E9", "#CC79A7", "#D55E00", "#009E73")
                        }
                    
                ### order data_plot
                if (input$sort_crit == "id") {data_plot = data_plot[order(data_plot$id, decreasing = input$sort_dir), ]}
                if (input$sort_crit == "tpx_date") {data_plot = data_plot[order(data_plot$tpx_date, decreasing = input$sort_dir), ]}
                    
                n = length(unique(data_plot$id))
    
                ### plot with relative or absolute time
                if (input$time == "relative") {
                        plot = ggplot(data_plot, aes(x=weeks, y=factor(id, levels=unique(id)), color=vis_type)) +
                                geom_line(size = 0.25, color = "black") +
                                geom_point(size = 3) +
                                ylab(paste("patients n =", n)) +
                                xlab("weeks") +
                                scale_color_manual(values = color_scheme,
                                                 name = "visit type",
                                                 breaks = c("Routine Transplantation", "Routine 4-6 weeks after Transplantation", "Routine 1 year after Transplantation",
                                                            "Symptom", "Symptom Follow-Up", "Death"),
                                                 labels = c("transplantation", "4-6 weeks after transplantation", "1 year after transplantation",
                                                            "symptom", "symptom follow-up", "death"))
                } else {
                        plot = ggplot(data_plot, aes(x=vis_date, y=factor(id, levels=unique(id)), color=vis_type)) +
                                geom_line(size = 0.25, color = "black") +
                                geom_point(size = 3) +
                                ylab(paste("patients n =", n)) +
                                xlab("date") +
                                scale_color_manual(values = color_scheme,
                                                 name = "visit type",
                                                 breaks = c("Routine Transplantation", "Routine 4-6 weeks after Transplantation", "Routine 1 year after Transplantation",
                                                            "Symptom", "Symptom Follow-Up", "Death"),
                                                 labels = c("transplantation", "4-6 weeks after transplantation", "1 year after transplantation",
                                                            "symptom", "symptom follow-up", "death"))
                  
                        }
                print(plot)
        }, height = 700)
}
)
