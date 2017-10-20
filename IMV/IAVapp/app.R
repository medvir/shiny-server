library(shiny)
library(tidyverse)
library(cowplot)

### Define the fields we want to save from the form
fields = c("sample_name", "IAV_titer")

outputDir = "/Users/Michael/Dropbox/Science/Repositories/shiny-server/IMV/IAVapp/responses"

saveData <- function(data) {
  data = t(data)
  # Create a unique file name
  fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
  # Write the file to the local system
  write.csv(
    x = data,
    file = file.path(outputDir, fileName), 
    row.names = FALSE, quote = TRUE
  )
}

loadData <- function() {
  # Read all the files into a list
  files = list.files(outputDir, full.names = TRUE)
  data = lapply(files, read.csv, stringsAsFactors = FALSE) 
  # Concatenate all data together into one data.frame
  data <- do.call(rbind, data)
  data
}


####################### only temporary data #######################
### Save a response
# ---- This is one of the two functions we will change for every storage type ----
# saveData <- function(data) {
#         data <- as.data.frame(t(data))
#         if (exists("responses")) {
#                 responses <<- rbind(responses, data)
#         } else {
#                 responses <<- data
#                 }
#         }

### Load all previous responses
# ---- This is one of the two functions we will change for every storage type ----
# loadData <- function() {
#         if (exists("responses")) {
#                 responses
#                 }
#         }
###################################################################







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
                        sidebarPanel(textInput("outdir", "Results directory"),
                                     hr(),
                                     numericInput("sample_name", "Sample Name", 17001, 17001, 17999),
                                     numericInput("IAV_titer", "IAV titer", 0, 0, 10000, 10),
                                     actionButton("submit", "Submit"),
                                     hr(),
                                     fileInput("results", "Upload IAV results"),
                                     width = 2
                                     ),
                        mainPanel(
                                tabsetPanel(
                                        tabPanel("Quantitative",
                                                 verticalLayout(h2("IAV titer by status"),
                                                                plotOutput("plot_quant", height = 800)
                                                 )),
                                        tabPanel("Qualitative",
                                                 verticalLayout(h2("IAV titer by status"),
                                                                plotOutput("plot_qual", height = 800)
                                                 )),
                                        tabPanel("Sex",
                                                 verticalLayout(h2("IAV titer in male and female individuals"),
                                                                plotOutput("plot_sex", height = 800)
                                                 )),
                                        tabPanel("Age",
                                                 verticalLayout(h2("IAV titer depending on age"),
                                                                plotOutput("plot_age", height = 800)
                                                 )),
                                        tabPanel("Replicates",
                                                 verticalLayout(h2("IAV titer measured by different students"),
                                                                plotOutput("plot_repli", height = 800)
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
                output$data_table = renderTable({
                        input$submit
                        plot_data()
                        })

                plot_data = reactive({
                        input$submit
                        as.data.frame(loadData()) %>%
                                mutate(sample_name = as.character(sample_name)) %>%
                                mutate(IAV_titer = as.numeric(as.character(IAV_titer))) %>%
                                mutate(result = ifelse(IAV_titer >= 20, "positive", "negative"))  %>%
                                left_join(., demo_data, by = "sample_name")
                        })
                
                plot_theme = theme(legend.position="none", axis.text=element_text(size = 15), axis.title=element_text(size = 20, face = "bold"))
                
                output$plot_quant = renderPlot({

                        print(input$outdir)
                        p = ggplot(plot_data(), aes(x = "", y = IAV_titer, colour = "black" , fill = "black")) +
                                geom_boxplot(outlier.color = "white", alpha = 0.1) +
                                geom_jitter(height = 0, width = 0.2, size = 4) +
                                facet_grid(vaccination_status ~ previous_influenza) +
                                panel_border() + background_grid(major = "y", minor = "") +
                                xlab("") +
                                ylab("IAV titer")
                        p = p + plot_theme
                        return(p)
                        })
                
                output$plot_qual = renderPlot({
                        p = ggplot(plot_data(), aes(x = result, color = "black", fill = "black")) +
                                geom_bar(alpha = 0.5) +
                                facet_grid(vaccination_status ~ previous_influenza) +
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
                
                output$plot_repli = renderPlot({
                        p = ggplot(plot_data(), aes(x = as.character(replicate), y = IAV_titer, color = sample_name, group = sample_name)) +
                                geom_line(size = 1) +
                                geom_jitter(height = 0, width = 0, size = 4) +
                                geom_text(aes(label = student),hjust = -0.25, vjust = -0.75) +
                                facet_grid(. ~ group) +
                                panel_border() + background_grid(major = "y", minor = "") +
                                xlab("Replicates") +
                                ylab("IAV titer")
                        p = p + plot_theme
                        return(p)
                        })
                
                output$plot_age = renderPlot({
                        p = ggplot(plot_data(), aes(x = age, y = IAV_titer)) +
                                geom_point(size = 4) +
                                geom_smooth(method = lm, se = FALSE, fullrange = TRUE) +
                                facet_grid(. ~ group) +
                                panel_border() + background_grid(major = "xy", minor = "") +
                                xlab("Age") +
                                ylab("IAV titer")
                        p = p + plot_theme
                        return(p)
                        })
        }
)
