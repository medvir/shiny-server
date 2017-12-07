### EBV app

library(shiny)
library(tidyverse)
library(cowplot)

plot_height = 800

### Load demographic data
demo_data = read_csv("demo_data.csv") %>%
        rename(sample_name = lab_code) %>%
        mutate(age = as.integer(2017 - year_of_birth)) %>%
        select("sample_name", "age", "sex", "ebv_infection", "ebv_confirmed")

ebv_data = read_csv("EBV_results.csv")

plot_data = left_join(ebv_data, demo_data, by = "sample_name") %>%
        filter(!(is.na(EBV_titer)))

### Shiny apps
shinyApp(
        ### ui
        ui = fluidPage(
                #titlePanel("EBV assay"),
                # sidebarLayout(
                #         sidebarPanel(
                #                      width = 2
                #                      ),
                #         mainPanel(
                                tabsetPanel(
                                        tabPanel("Qualitative",
                                                 verticalLayout(h2("EBV titer"),
                                                                plotOutput("plot_qual", height = plot_height)
                                                 )),
                                        tabPanel("Status",
                                                 verticalLayout(h2("EBV titer by status"),
                                                                plotOutput("plot_status", height = plot_height)
                                                 )),
                                        tabPanel("Age",
                                                 verticalLayout(h2("EBV titer by age"),
                                                                plotOutput("plot_age", height = plot_height)
                                                 )),
                                        tabPanel("Sex",
                                                 verticalLayout(h2("EBV titer by sex"),
                                                                plotOutput("plot_sex", height = plot_height)
                                                 ))
                                ),
                                hr(),
                                h2("Data table"),
                                tableOutput("data_table")
                        #         )
                        # )       
        ),
        
        ### server
        server = function(input, output, session) {
        
                output$data_table = renderTable({
                        plot_data
                        })
                
                plot_theme = theme(legend.position="none", axis.text=element_text(size = 20), axis.title=element_text(size = 20, face = "bold"), strip.text = element_text(size = 20))
                
                output$plot_qual = renderPlot({
                        p = ggplot(plot_data, aes(x = EBV_titer, colour = EBV_titer, fill = EBV_titer)) +
                                geom_bar(alpha = 0.5) +
                                panel_border() + background_grid(major = "y", minor = "") +
                                xlab("") +
                                ylab("Number of samples")
                        p = p + plot_theme
                        return(p)
                        })
                
                output$plot_status = renderPlot({
                        p = ggplot(plot_data, aes(x = EBV_titer, colour = EBV_titer, fill = EBV_titer)) +
                                geom_bar(alpha = 0.5) +
                                facet_grid(ebv_confirmed ~ ebv_infection) +
                                panel_border() + background_grid(major = "y", minor = "") +
                                xlab("") +
                                ylab("Number of samples")
                        p = p + plot_theme
                        return(p)
                })
                
                output$plot_age = renderPlot({
                        p = ggplot(plot_data, aes(x = age, y = EBV_titer)) +
                                geom_point(size = 4) +
                                geom_smooth(method = lm, se = FALSE, fullrange = TRUE) +
                                panel_border() + background_grid(major = "xy", minor = "") +
                                xlab("Age") +
                                ylab("EBV titer")
                        p = p + plot_theme
                        return(p)
                })
                
                output$plot_sex = renderPlot({
                        p = ggplot(plot_data, aes(x = EBV_titer, colour = EBV_titer, fill = EBV_titer)) +
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
