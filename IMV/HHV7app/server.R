### HHV7app

library(shiny)
library(tidyverse)
library(cowplot)

shinyServer(function(input, output, session) {
        
        ### data import and mutation
        HHV7_results = read_csv("HHV7_results.csv") %>%
                        mutate(group = substr(student, 1, 1)) %>%
                        mutate(result = ifelse(HHV7 > 1, "positive", "negative")) %>%
                        group_by(sample_name, sample_type) %>%
                        mutate(replicate = 1:n())
        
        demo_data = read_csv("demo_data.csv")
        
        ### ui inputs
        samples_found = reactive({HHV7_results %>% arrange(sample_name) %>% pull(sample_name) %>% unique()})
        groups_found = reactive({HHV7_results %>% arrange(group) %>% pull(group) %>% unique()})
        students_found = reactive({HHV7_results %>% arrange(student) %>% pull(student) %>% unique()})
        
        observeEvent(input$select_all_sample_types, {updateCheckboxGroupInput(session=session, "sample_type", selected = c("Blood", "Throat swab", "Urine"))})
        
        output$sample_selection = renderUI({selectInput("sample_name", "Sample Names", samples_found(), multiple = TRUE, selectize = FALSE, selected = samples_found())})
        observeEvent(input$select_all_samples, {updateSelectInput(session=session, "sample_name", selected = samples_found())})
        
        output$group_selection = renderUI({selectInput("group", "Groups", groups_found(), multiple = TRUE, selectize = FALSE, selected = groups_found())})
        observeEvent(input$select_all_groups, {updateSelectInput(session=session, "group", selected = groups_found())})
        
        output$student_selection = renderUI({selectInput("student", "Students", students_found(), multiple = TRUE, selectize = FALSE, selected = students_found())})
        observeEvent(input$select_all_students, {updateSelectInput(session=session, "student", selected = students_found())})
        
        ### data used for plots
        plot_data = reactive({
                full_join(HHV7_results, demo_data, by = "sample_name") %>%
                        filter(sample_type %in% input$sample_type) %>%
                        filter(sample_name %in% input$sample_name) %>%
                        filter(group %in% input$group) %>%
                        filter(student %in% input$student)
                })
        
        ### plot style
        plot_labels = list(ylab("HHV-7 copies/ml"))
        plot_theme = theme(legend.position="none", axis.text=element_text(size = 15), axis.title=element_text(size = 20, face = "bold"))
        
        ### output plots
        output$plot_quant = renderPlot({
                p = ggplot(plot_data(), aes(x = sample_type, y = HHV7, color = sample_type, fill = sample_type)) +
                        geom_boxplot(outlier.color = "white", alpha = 0.1) +
                        geom_jitter(height = 0, width = 0.2, size = 4) +
                        facet_grid(. ~ sample_type, scales = "free") +
                        panel_border() + background_grid(major = "y", minor = "") +
                        xlab("Sample type")
                p = p + plot_labels + plot_theme
                return(p)
                })
        
        output$plot_qual = renderPlot({
                p = ggplot(plot_data(), aes(x = result, color = sample_type, fill = sample_type)) +
                        geom_bar(alpha = 0.5) +
                        facet_grid(. ~ sample_type, scales = "free") +
                        panel_border() + background_grid(major = "y", minor = "") +
                        xlab("Result") +
                        ylab("Number of samples")
                p = p + plot_theme
                return(p)
                })
        
        output$plot_sex = renderPlot({
                p = ggplot(plot_data(), aes(x = sex, y = HHV7, color = sex, fill = sex)) +
                        geom_boxplot(outlier.color = "white", alpha = 0.1) +
                        geom_jitter(height = 0, width = 0.2, size = 4) +
                        facet_grid(. ~ sample_type) +
                        panel_border() + background_grid(major = "y", minor = "") +
                        xlab("Sex")
                p = p + plot_labels + plot_theme
                return(p)
                })
        
        output$plot_repli = renderPlot({
                p = ggplot(plot_data(), aes(x = as.character(replicate), y = HHV7, color = sample_name, group = sample_name)) +
                        geom_line(size = 1) +
                        geom_jitter(height = 0, width = 0, size = 4) +
                        geom_text(aes(label = student),hjust = -0.25, vjust = -0.75) +
                        facet_grid(. ~ sample_type) +
                        panel_border() + background_grid(major = "y", minor = "") +
                        xlab("Replicates")
                p = p + plot_labels + plot_theme
                return(p)
                })
        
        output$plot_symptoms = renderPlot({
                p = ggplot(plot_data(), aes(x = symptoms, y = HHV7, color = symptoms, fill = symptoms)) +
                        geom_boxplot(outlier.color = "white", alpha = 0.1) +
                        geom_jitter(height = 0, width = 0.2, size = 4) +
                        facet_grid(. ~ sample_type) +
                        panel_border() + background_grid(major = "y", minor = "") +
                        xlab("Symptoms")
                p = p + plot_labels + plot_theme
                return(p)
                })
        
        output$plot_age = renderPlot({
                p = ggplot(plot_data(), aes(x = age, y = HHV7)) +
                        geom_point(size = 4) +
                        geom_smooth(method = lm, se = FALSE, fullrange = TRUE) +
                        facet_grid(. ~ sample_type) +
                        panel_border() + background_grid(major = "xy", minor = "") +
                        xlab("Age")
                p = p + plot_labels + plot_theme
                return(p)
                })
        
        ### output data table
        output$data_table = renderTable({
                plot_data() %>%
                        arrange(sample_name, sample_type, student) %>%
                        select(sample_name, sample_type, age, sex, symptoms, replicate, HHV7, result, student)
                })
        
})