library(shiny)
library(tidyverse)
library(cowplot)

### Define the fields we want to save from the form
fields <- c("sample_name", "IAV_titer", "age", "sex", "vaccination status", "previous influenza")

### Save a response
# ---- This is one of the two functions we will change for every storage type ----
saveData <- function(data) {
        data <- as.data.frame(t(data))
        if (exists("responses")) {
                responses <<- rbind(responses, data)
        } else {
                responses <<- data
        }
}

### Load all previous responses
# ---- This is one of the two functions we will change for every storage type ----
loadData <- function() {
        if (exists("responses")) {
                responses
        }
}

### Load demographic data
demo_data = read_csv("demo_data.csv") %>%
        mutate(sample_name = as.character(sample_name))

### Shiny app
shinyApp(
        
        ### ui
        ui = fluidPage(
                titlePanel("IAV assay"),
                sidebarLayout(
                        sidebarPanel(textInput("sample_name", "Sample Name", ""),
                                     numericInput("IAV_titer", "IAV titer", 0, 0, 10000, 10),
                                     actionButton("submit", "Submit"),
                                     width = 2
                                     ),
                        mainPanel(
                                tabsetPanel(
                                        tabPanel("Quantitative",
                                                 verticalLayout(h2("IAV titer by status"),
                                                                plotOutput("plot_quant", height = 500)
                                                 )),
                                        tabPanel("Qualitative",
                                                 verticalLayout(h2("IAV titer by status"),
                                                                plotOutput("plot_qual", height = 500)
                                                 )),
                                        tabPanel("Sex",
                                                 verticalLayout(h2("IAV titer in male and female individuals"),
                                                                plotOutput("plot_sex", height = 500)
                                                 )),
                                        tabPanel("Age",
                                                 verticalLayout(h2("IAV titer depending on age"),
                                                                plotOutput("plot_age", height = 500)
                                                 )),
                                        tabPanel("Symptoms",
                                                 verticalLayout(h2("IAV titer depending on symptoms"),
                                                                plotOutput("plot_symptoms", height = 500)
                                                 )),
                                        tabPanel("Replicates",
                                                 verticalLayout(h2("IAV titer measured by different students"),
                                                                plotOutput("plot_repli", height = 500)
                                                 ))
                                ),
                                hr(),
                                h2("Data table"),
                                DT::dataTableOutput("data_table") #, width = 300)
                                )
                        )       
        ),
        
        ### server
        server = function(input, output, session) {
                
                ### Whenever a field is filled, aggregate all form data
                formData <- reactive({
                        data <- sapply(fields, function(x) input[[x]])
                        data
                        })
                
                ### When the Submit button is clicked, save the form data
                observeEvent(input$submit, {
                        saveData(formData())
                        })
                
                ### Show the previous responses (update with current response when Submit is clicked)
                output$data_table <- DT::renderDataTable({
                        input$submit
                        plot_data()
                        })

                plot_theme = theme(legend.position="none", axis.text=element_text(size = 15), axis.title=element_text(size = 20, face = "bold"))
                
                plot_data = reactive({
                        input$submit
                        as.data.frame(loadData()) %>%
                                mutate(sample_name = as.character(sample_name)) %>%
                                full_join(., demo_data, by = "sample_name") %>%
                                mutate(IAV_titer = as.numeric(IAV_titer))
                        })
                
                output$plot_quant = renderPlot({
                        p = ggplot(plot_data(), aes(x = "", y = IAV_titer, colour = "black" , fill = "green")) +
                                geom_boxplot(outlier.color = "white", alpha = 0.1) +
                                geom_jitter(height = 0, width = 0.2, size = 4) +
                                #facet_grid(. ~ sample_type, scales = "free") +
                                panel_border() + background_grid(major = "y", minor = "") +
                                xlab("") +
                                ylab("IAV titer")
                        p = p + plot_theme
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
                        p = ggplot(plot_data(), aes(x = sex, y = IAV_titer, color = sex, fill = sex)) +
                                geom_boxplot(outlier.color = "white", alpha = 0.1) +
                                geom_jitter(height = 0, width = 0.2, size = 4) +
                                facet_grid(. ~ sample_type) +
                                panel_border() + background_grid(major = "y", minor = "") +
                                xlab("Sex") +
                                ylab("IAV titer")
                        p = p + plot_theme
                        return(p)
                        })
                
                output$plot_repli = renderPlot({
                        p = ggplot(plot_data(), aes(x = as.character(replicate), y = IAV_titer, color = sample_name, group = sample_name)) +
                                geom_line(size = 1) +
                                geom_jitter(height = 0, width = 0, size = 4) +
                                geom_text(aes(label = student),hjust = -0.25, vjust = -0.75) +
                                facet_grid(. ~ sample_type) +
                                panel_border() + background_grid(major = "y", minor = "") +
                                xlab("Replicates") +
                                ylab("IAV titer")
                        p = p + plot_theme
                        return(p)
                        })
                
                output$plot_symptoms = renderPlot({
                        p = ggplot(plot_data(), aes(x = symptoms, y = IAV_titer, color = symptoms, fill = symptoms)) +
                                geom_boxplot(outlier.color = "white", alpha = 0.1) +
                                geom_jitter(height = 0, width = 0.2, size = 4) +
                                facet_grid(. ~ sample_type) +
                                panel_border() + background_grid(major = "y", minor = "") +
                                xlab("Symptoms") +
                                ylab("IAV titer")
                        p = p + plot_theme
                        return(p)
                        })
                
                output$plot_age = renderPlot({
                        p = ggplot(plot_data(), aes(x = age, y = IAV_titer)) +
                                geom_point(size = 4) +
                                geom_smooth(method = lm, se = FALSE, fullrange = TRUE) +
                                facet_grid(. ~ sample_type) +
                                panel_border() + background_grid(major = "xy", minor = "") +
                                xlab("Age") +
                                ylab("IAV titer")
                        p = p + plot_theme
                        return(p)
                        })
        }
)
