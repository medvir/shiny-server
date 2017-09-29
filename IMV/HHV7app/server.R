### HHV7app

library(shiny)
library(tidyr)
library(dplyr)
library(ggplot2)
library(cowplot)
library(readr)

shinyServer(function(input, output, session) {
        
        data <- reactive({
               read_csv("sample_results.csv") %>%
                        mutate(group = as.character(group))
                })
        
        observeEvent(input$all_types, {
                updateCheckboxGroupInput(session=session, "sample_type", selected = c("Blood", "Throat swab", "Urine"))
                })
        
        
        output$samples_found <- renderUI({
                s = data() %>% arrange(sample_name) %>% pull(sample_name) %>% unique() 
                selectInput("sample_name", "Sample Names", s, multiple=TRUE, selectize=FALSE, selected = s)
                })
        observeEvent(input$all_samples, {
                s = data() %>% arrange(sample_name) %>% pull(sample_name) %>% unique() 
                updateSelectInput(session=session, "sample_name", selected = s)
                })
        
        
        output$students_found <- renderUI({
                s = data() %>% arrange(student) %>% pull(student) %>% unique() 
                selectInput("student", "Student", s, multiple=TRUE, selectize=FALSE, selected = s)
                })
        observeEvent(input$all_students, {
                s = data() %>% arrange(student) %>% pull(student) %>% unique()
                updateSelectInput(session=session, "student", selected = s)
                })
        
        
        output$plot = renderPlot({
                req(input$sample_name)
                data = data() %>%
                        filter(sample_type %in% input$sample_type) %>%
                        filter(sample_name %in% input$sample_name) %>%
                        filter(student %in% input$student)
                if (input$comparison == "sample_type") {
                        p = ggplot(data, aes(x = sample_type, y = HHV7, color = sample_type))
                } else if (input$comparison == "sex") {
                        p = ggplot(data, aes(x = sex, y = HHV7, color = sex))
                } else if (input$comparison == "group") {
                        p = ggplot(data, aes(x = group, y = HHV7, color = group))
                } else if (input$comparison == "symptoms") {
                        p = ggplot(data, aes(x = group, y = HHV7, color = symptoms))
                } else if (input$comparison == "age") {
                        p = ggplot(data, aes(x = age, y = HHV7)) +
                                geom_point() +
                                geom_smooth(method = lm, se = FALSE, fullrange = TRUE)
                        }
                
                if (input$comparison != "age") {
                        p = p + geom_boxplot(outlier.color = "white") +
                                geom_jitter(height = 0, width = 0.2, size = 4)
                        }
                
                p = p + facet_grid(. ~ sample_type, scales = "free") +
                        ylab("HHV-7 copies/ml") +
                        theme(legend.position="none") +
                        theme(axis.text = element_text(size = 15)) +
                        theme(axis.title = element_text(size = 20, face = "bold")) 
                
                return(p)
                })
})
