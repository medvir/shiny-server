# IAV app

library(shiny)
library(tidyverse)
library(cowplot)
library(readxl)
library(forcats)

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
                                        tabPanel("Quantitative - by vaccine status",
                                                 verticalLayout(h2("IAV titer by vaccine status"),
                                                                plotOutput("plot_quant", height = plot_height)
                                                 )),
                                        tabPanel("Quantitative - by influenza status",
                                                 verticalLayout(h2("IAV titer of non vaccinated by influenza status"),
                                                                plotOutput("plot_quant_2", height = plot_height)
                                                 )),
                                        tabPanel("Sex",
                                                 verticalLayout(h2("IAV titer in male and female individuals"),
                                                                plotOutput("plot_sex", height = plot_height)
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
                        rbind(read_excel("2018-11-30_HI-titers.xlsx")) %>%
                                rename(sample_name = number) %>%
                                gather(key = measurment, value = titer, -sample_name, -virus, -virus_strain) %>%
                                select(-measurment) %>%
                                filter(!(is.na(titer))) %>%
                                group_by(sample_name, virus) %>%
                                mutate(IAV_titer = mean(titer)) %>%
                                select(-titer) %>%
                                sample_n(1) %>%
                                ungroup() %>%
                                mutate(result = ifelse(IAV_titer >= 4, "positive", "negative")) %>%
                                left_join(., demo_data, by = "sample_name") %>%
                                mutate(vaccinated_status = case_when(
                                        influenza_status == 1 ~ "never vaccinated",
                                        influenza_status == 2 ~ "vaccinated",
                                        influenza_status == 3 ~ "never vaccinated",
                                        influenza_status == 4 ~ "vaccinated",
                                        TRUE ~ "other")) %>%
                                mutate(influenza_status = case_when(
                                        influenza_status == 1 ~ "never influenza",
                                        influenza_status == 2 ~ "never influenza",
                                        influenza_status == 3 ~ "influenza",
                                        influenza_status == 4 ~ "influenza",
                                        TRUE ~ "other")) 
                })
                
                output$data_table = renderTable({
                        req(input$results$datapath)
                        results()
                        })
                
                plot_theme = theme(legend.position="none", axis.text=element_text(size = 25), axis.title=element_text(size = 20, face = "bold"), strip.text = element_text(size = 20))
                
                output$plot_quant = renderPlot({
                        req(input$results$datapath)
                        p = ggplot(results(), aes(x = fct_relevel(virus, "current H3", "current H1", "older H1"), y = log2(IAV_titer), colour = vaccinated_status , fill = vaccinated_status)) +
                                geom_boxplot(outlier.color = "white", alpha = 0.3) +
                                geom_jitter(height = 0, width = 0.2, size = 4) +
                                facet_grid(. ~ vaccinated_status, scales = "free") +
                                panel_border() + background_grid(major = "y", minor = "") +
                                xlab("") +
                                ylab("IAV titer (log2)") +
                                scale_color_manual(values = c("#E69F00", "#56B4E9")) +
                                scale_fill_manual(values = c("#E69F00", "#56B4E9")) +
                                geom_hline(aes(yintercept = log2(40)), linetype = 2) +
                                geom_hline(aes(yintercept = log2(10)), linetype = 2)
                        p = p + plot_theme
                        return(p)
                })
                
                output$plot_quant_2 = renderPlot({
                        req(input$results$datapath)
                        p = 
                                results() %>%
                                filter(vaccinated_status == "never vaccinated") %>%
                                ggplot(aes(x = fct_relevel(virus, "current H3", "current H1", "older H1"), y = log2(IAV_titer), colour = vaccinated_status , fill = vaccinated_status)) +
                                geom_boxplot(outlier.color = "white", alpha = 0.3) +
                                geom_jitter(height = 0, width = 0.2, size = 4) +
                                facet_grid(. ~ influenza_status, scales = "free") +
                                panel_border() + background_grid(major = "y", minor = "") +
                                xlab("") +
                                ylab("IAV titer (log2)") +
                                scale_color_manual(values = c("#E69F00", "#56B4E9")) +
                                scale_fill_manual(values = c("#E69F00", "#56B4E9")) +
                                geom_hline(aes(yintercept = log2(40)), linetype = 2) +
                                geom_hline(aes(yintercept = log2(10)), linetype = 2)
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
                                scale_color_manual(values = c("#009E73", "#0072B2")) +
                                scale_fill_manual(values = c("#009E73", "#0072B2")) +
                                xlab("Sex") +
                                ylab("IAV titer (log2)")
                        p = p + plot_theme
                        return(p)
                })
        }
)