### EBV app

library(shiny)
library(tidyverse)
library(cowplot)

### Load demographic data
demo_data = read_csv("demo_data.csv") %>%
        rename(sample_name = lab_code) %>%
        mutate(age = as.integer(2017 - year_of_birth)) %>%
        select("sample_name", "age", "sex", "ebv_infection", "ebv_confirmed")

### Shiny apps
shinyApp(
        ### ui
        ui = fluidPage(
                titlePanel("EBV assay"),
                sidebarLayout(
                        sidebarPanel(fileInput("results", "Upload EBV results"),
                                     width = 2
                                     ),
                        mainPanel(
                                tabsetPanel(
                                        tabPanel("Qualitative",
                                                 verticalLayout(h2("EBV titer"),
                                                                plotOutput("plot_qual", height = 600)
                                                 )),
                                        tabPanel("Status",
                                                 verticalLayout(h2("EBV titer by status"),
                                                                plotOutput("plot_status", height = 600)
                                                 )),
                                        tabPanel("Age",
                                                 verticalLayout(h2("EBV titer by age"),
                                                                plotOutput("plot_age", height = 600)
                                                 )),
                                        tabPanel("Sex",
                                                 verticalLayout(h2("EBV titer by sex"),
                                                                plotOutput("plot_sex", height = 600)
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
                
                plot_data = reactive({
                        read_csv(input$results$datapath) %>%
                                mutate(EBV_titer = as.character(EBV_titer)) %>%
                                left_join(., demo_data, by = "sample_name") %>%
                                filter(!(is.na(EBV_titer)))
                        })
        
                output$data_table = renderTable({
                        req(input$results$datapath)
                        plot_data()
                        })
                
                plot_theme = theme(legend.position="none", axis.text=element_text(size = 15), axis.title=element_text(size = 20, face = "bold"))
                
                output$plot_qual = renderPlot({
                        req(input$results$datapath)
                        p = ggplot(plot_data(), aes(x = EBV_titer, colour = EBV_titer, fill = EBV_titer)) +
                                geom_bar(alpha = 0.5) +
                                panel_border() + background_grid(major = "y", minor = "") +
                                xlab("") +
                                ylab("Number of samples")
                        p = p + plot_theme
                        return(p)
                        })
                
                output$plot_status = renderPlot({
                        req(input$results$datapath)
                        p = ggplot(plot_data(), aes(x = EBV_titer, colour = EBV_titer, fill = EBV_titer)) +
                                geom_bar(alpha = 0.5) +
                                facet_grid(ebv_confirmed ~ ebv_infection) +
                                panel_border() + background_grid(major = "y", minor = "") +
                                xlab("") +
                                ylab("Number of samples")
                        p = p + plot_theme
                        return(p)
                })
                
                output$plot_age = renderPlot({
                        req(input$results$datapath)
                        p = ggplot(plot_data(), aes(x = age, y = EBV_titer)) +
                                geom_point(size = 4) +
                                geom_smooth(method = lm, se = FALSE, fullrange = TRUE) +
                                panel_border() + background_grid(major = "xy", minor = "") +
                                xlab("Age") +
                                ylab("EBV titer")
                        p = p + plot_theme
                        return(p)
                })
                
                output$plot_sex = renderPlot({
                        req(input$results$datapath)
                        p = ggplot(plot_data(), aes(x = EBV_titer, colour = EBV_titer, fill = EBV_titer)) +
                                geom_bar(alpha = 0.5) +
                                facet_grid(. ~ sex) +
                                panel_border() + background_grid(major = "y", minor = "") +
                                xlab("") +
                                ylab("Number of samples")
                        p = p + plot_theme
                        return(p)
                })
                
        }
)
