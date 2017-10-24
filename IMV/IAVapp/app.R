library(shiny)
library(tidyverse)
library(cowplot)

### Load demographic data
demo_data = read_csv("demo_data.csv") %>%
        mutate(sample_name = as.character(sample_name)) %>%
        mutate(group = case_when(
                vaccination_status == "yes" & previous_influenza == "yes" ~ "group 1",
                vaccination_status == "yes" & previous_influenza == "no" ~ "group 2",
                vaccination_status == "no" & previous_influenza == "yes" ~ "group 3",
                vaccination_status == "no" & previous_influenza == "no" ~ "group 4",
                TRUE ~ "NA"
                )) %>%
        mutate(previous_influenza = ifelse(previous_influenza == "yes", "influenza experienced", "never had influenza")) %>%
        mutate(vaccination_status = ifelse(vaccination_status == "yes", "vaccinated", "never vaccinated")) %>%
        select(-symptoms)

### Shiny apps
shinyApp(
        ### ui
        ui = fluidPage(
                titlePanel("IAV assay"),
                sidebarLayout(
                        sidebarPanel(fileInput("results", "Upload IAV results"),
                                     hr(),
                                     checkboxGroupInput("sex", "Sex", choices = c("male", "female"), selected = c("male", "female")),
                                     width = 3
                                     ),
                        mainPanel(
                                tabsetPanel(
                                        tabPanel("Quantitative",
                                                 verticalLayout(h2("IAV titer by status"),
                                                                plotOutput("plot_quant", height = 600)
                                                 )),
                                        tabPanel("Qualitative",
                                                 verticalLayout(h2("IAV titer by status"),
                                                                plotOutput("plot_qual", height = 600)
                                                 )),
                                        tabPanel("Sex",
                                                 verticalLayout(h2("IAV titer in male and female individuals"),
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

                results = reactive({read_csv(input$results$datapath)})
                
                plot_data = reactive({
                        results() %>%
                                mutate(sample_name = as.character(sample_name)) %>%
                                mutate(IAV_titer = as.numeric(as.character(IAV_titer))) %>%
                                mutate(result = ifelse(IAV_titer >= 20, "positive", "negative"))  %>%
                                left_join(., demo_data, by = "sample_name") %>%
                                filter(sex %in% input$sex)
                        })
        
                output$data_table = renderTable({
                        req(input$results$datapath)
                        plot_data()
                        })
                
                plot_theme = theme(legend.position="none", axis.text=element_text(size = 15), axis.title=element_text(size = 20, face = "bold"))
                
                output$plot_quant = renderPlot({
                        req(input$results$datapath)
                        p = ggplot(plot_data(), aes(x = group, y = IAV_titer, colour = group , fill = group)) +
                                geom_boxplot(outlier.color = "white", alpha = 0.1) +
                                geom_jitter(height = 0, width = 0.2, size = 4) +
                                facet_grid(. ~ group, scales = "free") +
                                panel_border() + background_grid(major = "y", minor = "") +
                                xlab("") +
                                ylab("IAV titer")
                        p = p + plot_theme
                        return(p)
                        })
                
                output$plot_qual = renderPlot({
                        p = ggplot(plot_data(), aes(x = result, colour = result , fill = result)) +
                                geom_bar(alpha = 0.5) +
                                facet_grid(. ~ group) +
                                panel_border() + background_grid(major = "y", minor = "") +
                                xlab("") +
                                ylab("Number of samples")
                        p = p + plot_theme
                        return(p)
                        })
                
                output$plot_sex = renderPlot({
                        p = ggplot(plot_data(), aes(x = sex, y = IAV_titer, color = sex, fill = sex)) +
                                geom_boxplot(outlier.color = "white", alpha = 0.1) +
                                geom_jitter(height = 0, width = 0.2, size = 4) +
                                facet_grid(. ~ group) +
                                panel_border() + background_grid(major = "y", minor = "") +
                                xlab("Sex") +
                                ylab("IAV titer")
                        p = p + plot_theme
                        return(p)
                        })
        }
)