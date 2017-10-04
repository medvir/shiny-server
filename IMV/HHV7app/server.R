### HHV7app

library(shiny)
library(tidyverse)

shinyServer(function(input, output, session) {
        
        data = reactive({read_csv("sample_results.csv") %>% mutate(group = as.character(group))})
        
        samples_found = reactive({data() %>% arrange(sample_name) %>% pull(sample_name) %>% unique()})
        students_found = reactive({data() %>% arrange(student) %>% pull(student) %>% unique()})
        
        observeEvent(input$select_all_sample_types, {updateCheckboxGroupInput(session=session, "sample_type", selected = c("Blood", "Throat swab", "Urine"))})
        
        output$sample_selection = renderUI({selectInput("sample_name", "Sample Names", samples_found(), multiple = TRUE, selectize = FALSE, selected = samples_found())})
        observeEvent(input$select_all_samples, {updateSelectInput(session=session, "sample_name", selected = samples_found())})
        
        output$student_selection = renderUI({selectInput("student", "Students", students_found(), multiple = TRUE, selectize = FALSE, selected = students_found())})
        observeEvent(input$select_all_students, {updateSelectInput(session=session, "student", selected = students_found())})
        
        plot_data = reactive({data() %>%
                        filter(sample_type %in% input$sample_type) %>%
                        filter(sample_name %in% input$sample_name) %>%
                        filter(student %in% input$student)
                })
        
        plot_labels = list(ylab("HHV-7 copies/ml"))
        plot_theme = theme(legend.position="none", axis.text=element_text(size = 15), axis.title=element_text(size = 20, face = "bold"))
        
        output$plot_sample = renderPlot({
                p = ggplot(plot_data(), aes(x = sample_type, y = HHV7, color = sample_type, fill = sample_type)) +
                        geom_boxplot(outlier.color = "white", alpha = 0.1) +
                        geom_jitter(height = 0, width = 0.2, size = 4) +
                        facet_grid(. ~ sample_type, scales = "free") +
                        xlab("Sample type")
                p = p + plot_labels + plot_theme
                return(p)
                })
        
        output$plot_sex = renderPlot({
                p = ggplot(plot_data(), aes(x = sex, y = HHV7, color = sex, fill = sex)) +
                        geom_boxplot(outlier.color = "white", alpha = 0.1) +
                        geom_jitter(height = 0, width = 0.2, size = 4) +
                        facet_grid(. ~ sample_type) +
                        xlab("Sex")
                p = p + plot_labels + plot_theme
                return(p)
                })
        
        output$plot_group = renderPlot({
                p = ggplot(plot_data(), aes(x = group, y = HHV7, color = group, fill = group)) +
                        geom_boxplot(outlier.color = "white", alpha = 0.1) +
                        geom_jitter(height = 0, width = 0.2, size = 4) +
                        facet_grid(. ~ sample_type) +
                        xlab("Student group")
                p = p + plot_labels + plot_theme
                return(p)
                })
        
        output$plot_symptoms = renderPlot({
                #p = ggplot
                #return(p)
                #p = p + plot_labels + plot_theme
                })
        
        output$plot_age = renderPlot({
                p = ggplot(plot_data(), aes(x = age, y = HHV7)) +
                        geom_point(size = 4) +
                        geom_smooth(method = lm, se = FALSE, fullrange = TRUE) +
                        facet_grid(. ~ sample_type) +
                        xlab("Age")
                p = p + plot_labels + plot_theme
                return(p)
                })
})