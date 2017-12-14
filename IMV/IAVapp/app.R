# IAV app

library(shiny)
library(tidyverse)
library(cowplot)
library(readxl)

plot_height = 800

### Load demographic data
demo_data = read_csv("demo_data.csv") %>%
        rename(sample_name = lab_code) %>%
        mutate(age = as.integer(2017 - year_of_birth)) %>%
        select("sample_name", "age", "sex", "influenza_status", "last_influenza", "influenza_confirmed")

### Shiny apps
shinyApp(
        ### ui
        ui = fluidPage(
                #titlePanel("IAV assay"),
                sidebarLayout(
                        sidebarPanel(fileInput("results", "Upload IAV results"),
                                     # hr(),
                                     # checkboxGroupInput("sex", "Sex", choices = c("male", "female"), selected = c("male", "female")),
                                     width = 2
                                     ),
                        mainPanel(
                                tabsetPanel(
                                        tabPanel("Quantitative",
                                                 verticalLayout(h2("IAV titer by status"),
                                                                plotOutput("plot_quant", height = plot_height)
                                                 )),
                                        tabPanel("Qualitative",
                                                 verticalLayout(h2("IAV titer by status"),
                                                                plotOutput("plot_qual", height = plot_height)
                                                 )),
                                        tabPanel("Sex",
                                                 verticalLayout(h2("IAV titer in male and female individuals"),
                                                                plotOutput("plot_sex", height = plot_height)
                                                 )),
                                        tabPanel("Age",
                                                verticalLayout(h2("IAV titer by age"),
                                                               plotOutput("plot_age", height = plot_height)
                                                ))
                                ),
                                hr(),
                                h2("Data table"),
                                tableOutput("data_table")
                                )
                        )       
        ),
        
        ### server
        server = function(input, output, session) {

                results = reactive({
                        rbind(read_excel("analysis_HI_data.xlsx", range = "A1:F31"), read_excel("analysis_HI_data.xlsx", range = "I1:N31")) %>%
                                rename(sample_name = number) %>%
                                gather(key = measurment, value = titer, -sample_name, -virus) %>%
                                select(-measurment) %>%
                                filter(!(is.na(titer))) %>%
                                group_by(sample_name, virus) %>%
                                mutate(IAV_titer = mean(titer)) %>%
                                select(-titer) %>%
                                sample_n(1) %>%
                                ungroup() %>%
                                mutate(result = ifelse(IAV_titer >= 4, "positive", "negative")) %>%
                                left_join(., demo_data, by = "sample_name") %>%
                                mutate(influenza_status = case_when(
                                        influenza_status == 1 ~ "never influenza, never vaccinated",
                                        influenza_status == 2 ~ "never influenza, vaccinated",
                                        influenza_status == 3 ~ "influenza, never vaccinated",
                                        influenza_status == 4 ~ "influenza, vaccinated",
                                        TRUE ~ "other")
                                       )
                })
                
                output$data_table = renderTable({
                        req(input$results$datapath)
                        results()
                        })
                
                plot_theme = theme(legend.position="none", axis.text=element_text(size = 25), axis.title=element_text(size = 20, face = "bold"), strip.text = element_text(size = 20))
                
                output$plot_quant = renderPlot({
                        req(input$results$datapath)
                        p = ggplot(results(), aes(x = virus, y = log2(IAV_titer), colour = influenza_status , fill = influenza_status)) +
                                geom_boxplot(outlier.color = "white", alpha = 0.3) +
                                geom_jitter(height = 0, width = 0.2, size = 4) +
                                facet_grid(. ~ influenza_status, scales = "free") +
                                panel_border() + background_grid(major = "y", minor = "") +
                                xlab("") +
                                ylab("IAV titer (log2)")
                        p = p + plot_theme
                        return(p)
                })
                
                output$plot_qual = renderPlot({
                        req(input$results$datapath)
                        p = ggplot(results(), aes(x = result, colour = result , fill = result)) +
                                geom_bar(alpha = 0.5) +
                                facet_grid(virus ~ influenza_status) +
                                panel_border() + background_grid(major = "y", minor = "") +
                                xlab("") +
                                ylab("Number of samples")
                        p = p + plot_theme
                        return(p)
                })
                
                output$plot_sex = renderPlot({
                        req(input$results$datapath)
                        p = ggplot(results(), aes(x = sex, y = log2(IAV_titer), color = sex, fill = sex)) +
                                geom_boxplot(outlier.color = "white", alpha = 0.3) +
                                geom_jitter(height = 0, width = 0.2, size = 4) +
                                #facet_grid(virus ~ influenza_status) +
                                panel_border() + background_grid(major = "y", minor = "") +
                                xlab("Sex") +
                                ylab("IAV titer (log2)")
                        p = p + plot_theme
                        return(p)
                })
                
                output$plot_age = renderPlot({
                        req(input$results$datapath)
                        p = ggplot(results(), aes(x = age, y = log2(IAV_titer))) +
                                geom_point(size = 4) +
                                geom_smooth(method = lm, se = FALSE, fullrange = TRUE) +
                                #facet_grid(virus ~ influenza_status) +
                                panel_border() + background_grid(major = "y", minor = "") +
                                xlab("Age") +
                                ylab("IAV titer (log2)")
                        p = p + plot_theme
                        return(p)
                })
        }
)