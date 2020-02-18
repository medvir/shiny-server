### HHV7app

library(shiny)
library(tidyverse)
library(cowplot)

shinyServer(function(input, output, session) {
        
        ### data import and mutation
        HHV7_results = read_csv("PCR_results.csv") %>%
                rename(sample_name = sample) %>%
                rename(HHV7 = mean_conc_dupl) %>%
                mutate(HHV7 = HHV7 *1000) %>%
                mutate(course = substr(student, 1, 1)) %>%
                mutate(sample_type = substr(sample_name, 1, 2)) %>%
                mutate(sample_name = substr(sample_name, 3, 7)) %>%
                filter(student != "0_00_00") %>%
                filter(valid) %>%
                filter(sd != 0 | is.na(sd)) %>%
                group_by(sample_name, sample_type, student) %>%
                sample_n(1) %>%
                group_by(sample_name, sample_type) %>%
                mutate(replicate = 1:n()) %>%
                mutate(mean_HHV7 = mean(HHV7)) %>%
                mutate(result = ifelse(mean_HHV7 > 0, "positive", "negative"))

        
        demo_data = read_csv("demo_data.csv") %>%
                rename(sample_name = lab_code) %>%
                mutate(age = 2018 - year_of_birth) %>%
                mutate(symptoms = list_respiratory_infection) %>%
                select(sample_name, age, sex, symptoms) %>%
                mutate(symptoms = ifelse(symptoms == "common cold signs", "common cold", symptoms))
        
        ### ui inputs
        samples_found = reactive({HHV7_results %>% arrange(sample_name) %>% pull(sample_name) %>% unique()})
        courses_found = reactive({HHV7_results %>% arrange(course) %>% pull(course) %>% unique()})
        students_found = reactive({HHV7_results %>% arrange(student) %>% pull(student) %>% unique()})
        
        observeEvent(input$select_all_sample_types, {updateCheckboxGroupInput(session=session, "sample_type", selected = c("Blood", "Throat swab", "Urine"))})
        
        output$sample_selection = renderUI({selectInput("sample_name", "Sample Names", samples_found(), multiple = TRUE, selectize = FALSE, selected = samples_found())})
        observeEvent(input$select_all_samples, {updateSelectInput(session=session, "sample_name", selected = samples_found())})
        
        output$course_selection = renderUI({selectInput("course", "Courses", courses_found(), multiple = TRUE, selectize = FALSE, selected = courses_found())})
        observeEvent(input$select_all_courses, {updateSelectInput(session=session, "course", selected = courses_found())})
        
        output$student_selection = renderUI({selectInput("student", "Students", students_found(), multiple = TRUE, selectize = FALSE, selected = students_found())})
        observeEvent(input$select_all_students, {updateSelectInput(session=session, "student", selected = students_found())})
        
        ### data used for plots
        plot_data = reactive({
                right_join(HHV7_results, demo_data, by = "sample_name") %>%
                        filter(sample_type %in% input$sample_type) %>%
                        filter(sample_name %in% input$sample_name) %>%
                        # filter(course %in% input$course) %>%
                        filter(student %in% input$student)
                })
        
        ### plot style
        plot_labels = list(ylab("HHV-7 copies/ml"))
        plot_theme = theme(legend.position="none", axis.text=element_text(size = 30), axis.title=element_text(size = 30, face = "bold"), strip.text = element_text(size = 30))
        
        ### output plots
        output$plot_quant = renderPlot({
                p = plot_data() %>%
                        group_by(sample_name, sample_type) %>%
                        sample_n(1) %>%
                        ggplot(aes(x = sample_type, y = mean_HHV7, color = sample_type, fill = sample_type)) +
                        geom_boxplot(outlier.color = "white", alpha = 0.5) +
                        geom_jitter(height = 0, width = 0.2, size = 4) +
                        facet_grid(. ~ sample_type, scales = "free") +
                        panel_border() + background_grid(major = "y", minor = "") +
                        xlab("Sample type")
                p = p + plot_labels + plot_theme + theme_set(theme_cowplot())
                return(p)
                })
        
        output$plot_qual = renderPlot({
                p = plot_data() %>%
                        group_by(sample_name, sample_type) %>%
                        sample_n(1) %>%
                        ggplot(aes(x = result, color = sample_type, fill = sample_type)) +
                        geom_bar(alpha = 0.5) +
                        facet_grid(. ~ sample_type, scales = "free") +
                        panel_border() + background_grid(major = "y", minor = "") +
                        xlab("Result") +
                        ylab("Number of samples")
                p = p + plot_theme + theme_set(theme_cowplot())
                return(p)
                })
        
        output$plot_sex = renderPlot({
                p = plot_data() %>%
                        group_by(sample_name, sample_type) %>%
                        sample_n(1) %>%
                        ggplot(aes(x = sex, y = mean_HHV7, color = sex, fill = sex)) +
                        geom_boxplot(outlier.color = "white", alpha = 0.5) +
                        geom_jitter(height = 0, width = 0.2, size = 4) +
                        facet_grid(. ~ sample_type) +
                        panel_border() + background_grid(major = "y", minor = "") +
                        xlab("Sex")
                p = p + plot_labels + plot_theme + theme_set(theme_cowplot())
                return(p)
                })
        
        output$plot_repli = renderPlot({
                p = ggplot(plot_data(), aes(x = as.character(replicate), y = HHV7, color = sample_name, group = sample_name)) +
                        geom_line(size = 1) +
                        geom_jitter(height = 0, width = 0, size = 4) +
                        geom_text(aes(label = paste0(student, "_", sample_name), hjust = -0.25, vjust = -0.75)) +
                        facet_grid(. ~ sample_type) +
                        panel_border() + background_grid(major = "y", minor = "") +
                        xlab("Replicates")
                p = p + plot_labels + plot_theme + theme_set(theme_cowplot())
                return(p)
                })
        
        output$plot_symptoms = renderPlot({
                p = plot_data() %>%
                        group_by(sample_name, sample_type) %>%
                        sample_n(1) %>%
                        ggplot(aes(x = symptoms, y = mean_HHV7, color = symptoms, fill = symptoms)) +
                        geom_boxplot(outlier.color = "white", alpha = 0.5) +
                        geom_jitter(height = 0, width = 0.2, size = 4) +
                        facet_grid(. ~ sample_type) +
                        panel_border() + background_grid(major = "y", minor = "") +
                        xlab("Symptoms") +
                        theme(axis.text.x = element_text(angle = 90, hjust = 1))
                p = p + plot_labels + plot_theme + theme_set(theme_cowplot())
                return(p)
                })
        
        output$plot_age = renderPlot({
                p = plot_data() %>%
                        group_by(sample_name, sample_type) %>%
                        sample_n(1) %>%
                        ggplot(aes(x = age, y = mean_HHV7)) +
                        geom_point(size = 4) +
                        geom_smooth(method = lm, se = FALSE, fullrange = TRUE) +
                        facet_grid(. ~ sample_type) +
                        panel_border() + background_grid(major = "xy", minor = "") +
                        xlab("Age")
                p = p + plot_labels + plot_theme + theme_set(theme_cowplot())
                return(p)
                })
        
        ### output data table
        output$data_table = renderTable({
                print(plot_data())
                plot_data() %>%
                        arrange(sample_name, sample_type, student) %>%
                        select(sample_name, sample_type, age, sex, symptoms, replicate, HHV7, mean_HHV7, result, student)
                })
        
})
