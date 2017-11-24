library(shiny)
library(tidyverse)
library(ape)
library(seqinr)
source('rainbowtree.r')

unrootedNJtree <- function(alignment, type) {
        # this function requires the ape and seqinR packages
        # define a function for making a tree
        makemytree <- function(alignmentmat){
                alignment <- ape::as.alignment(alignmentmat)
                if (type == "protein"){
                        mydist <- dist.alignment(alignment)
                }
                else if (type == "DNA"){
                        alignmentbin <- as.DNAbin(alignment)
                        mydist <- dist.dna(alignmentbin)
                }
                mytree <- nj(mydist)
                mytree <- makeLabel(mytree, space = "") # get rid of spaces in tip names.
                return(mytree)
        }
        # infer a tree
        mymat  <- as.matrix.alignment(alignment)
        mytree <- makemytree(mymat)
        # bootstrap the tree
        myboot <- boot.phylo(mytree, mymat, makemytree)
        # plot the tree:
        # plot.phylo(mytree,type="u")   # plot the unrooted phylogenetic tree
        # nodelabels(myboot,cex=0.7)    # plot the bootstrap values
        mytree$node.label <- myboot   # make the bootstrap values be the node labels
        return(mytree)
}


rootedNJtree <- function(alignment, theoutgroup, type) {
        # this function requires the ape and seqinR packages
        # define a function for making a tree
        makemytree <- function(alignmentmat, outgroup = theoutgroup) {
                alignment <- ape::as.alignment(alignmentmat)
                if (type == "protein"){
                        mydist <- dist.alignment(alignment)
                }
                else if (type == "DNA"){
                        alignmentbin <- as.DNAbin(alignment)
                        mydist <- dist.dna(alignmentbin)
                }
                mytree <- nj(mydist)
                mytree <- makeLabel(mytree, space = "") # get rid of spaces in tip names.
                myrootedtree <- root(mytree, outgroup, r = TRUE)
                return(myrootedtree)
        }
        # infer a tree
        mymat  <- as.matrix.alignment(alignment)
        myrootedtree <- makemytree(mymat, outgroup = theoutgroup)
        # bootstrap the tree
        myboot <- boot.phylo(myrootedtree, mymat, makemytree)
        # plot the tree:
        # plot.phylo(myrootedtree, type="p")  # plot the rooted phylogenetic tree
        # nodelabels(myboot,cex=0.7)          # plot the bootstrap values
        myrootedtree$node.label <- myboot   # make the bootstrap values be the node labels
        return(myrootedtree)
}


#-#-#-#-#-#-# UI #-#-#-#-#-#-#
ui <- fluidPage(
        
        ### Title panel
        titlePanel("Rainbow Tree"),
        
        ### Sidebar panel
        sidebarLayout(
                sidebarPanel(
                        fileInput("msaFile", "Upload multiple sequence alignment (fasta)",
                                  accept = c("fasta", "fna", "fas")),
                        actionButton("sample_data", "Use sample data"),
                        #textInput("treetitle", "Tree title"), ### not using tree title at the moment
                        hr(),
                        radioButtons("treetype", "Tree Type",
                                     choiceNames = c("phylogram", "star", "fan"),
                                     choiceValues = c("p", "u", "fan"),
                                     inline = TRUE),
                        hr(),
                        uiOutput("select_outgroup"),
                        hr(),
                        radioButtons("label", "Labels",
                                     choices = c("seqname", "category", "symbol", "none"),
                                     selected = "seqname",
                                     inline = TRUE),
                        uiOutput("label4ut"),
                        hr(),
                        radioButtons("cat", "Define categories according to",
                                     choiceNames = c("first character(s) in sequence name", "field in delimited sequence names", "an uploaded table"),
                                     choiceValues = c("cat_fc", "cat_field", "cat_table")),
                        uiOutput("cat_fc"),
                        uiOutput("cat_delim"),
                        uiOutput("cat_field"),
                        uiOutput("cat_table"),
                        hr(),
                        sliderInput("branchwidth", "Branch width", 1, 10, 2, 1, ticks = FALSE),
                        sliderInput("textsize", "Text size", 1, 5, 1.5, .25, ticks = FALSE),
                        hr(),
                        radioButtons("legend", "Legend",
                                     choices = c("vertical", "horizontal", "none"),
                                     selected = "vertical",
                                     inline = TRUE),
                        uiOutput("legendpos"),
                        hr(),
                        h5("Options"),
                        checkboxInput("show.node.lab", "Show node labels"),
                        checkboxInput("color.int.edges", "Color internal edges"),
                        width = 3
                ),
                
                ### Main Panel
                mainPanel(
                        plotOutput("rainbowTreePlot", height = "auto"),
                        downloadButton("plotDownload", label = "Download rainbow tree plot (pdf)"),
                        downloadButton("fileDownload", label = "Download tree file (nwy)")
                )
        )
)


#-#-#-#-#-#-# SERVER #-#-#-#-#-#-#
server <- function(input, output, session) {
        
        ### Sample Data
        values = reactiveValues(upload_state = NULL)
        observeEvent(input$msaFile, {values$upload_state <- "uploaded"})
        observeEvent(input$sample_data, {values$upload_state <- "sample_data"})
        
        file_input <- reactive({
                if (is.null(values$upload_state)) {
                        return(NULL)
                } else if (values$upload_state == "uploaded") {
                        return(input$msaFile$datapath)
                } else if (values$upload_state == "sample_data") {
                        return("data/test_msa.fasta")
                }
        })
        
        msa_in <- reactive({
                req(file_input())
                seqinr::read.alignment(file = file_input(), format = "fasta")
        })
        
        
        ###  Trees
        unrooted_tree <- reactive({
                # no req here because input$upload already required in msa_in
                msa_in() %>%
                        unrootedNJtree(type = "DNA")
        })
        
        rooted_tree <- reactive({
                # no req here because input$upload already required in msa_in
                msa_in() %>%
                        rootedNJtree(theoutgroup = input$select_outgroup, type = "DNA")
        })
        
        
        
        
        
        
        ### plot for rooted and unrooted tree
        rainbowtreeplot <- reactive({
                req(file_input())
                if (input$select_outgroup == "none") {
                        tmptree = unrooted_tree()
                } else {
                        tmptree = rooted_tree()
                }
                rainbowtree(tmptree,
                            treetype = input$treetype,
                            treetitle = input$treetitle,
                            label = input$label,
                            label4ut = input$label4ut,
                            legend = input$legend,
                            legendpos = input$legendpos,
                            branchwidth = input$branchwidth,
                            outgroup = input$select_outgroup,
                            color.int.edges = input$color.int.edges,
                            show.node.lab = input$show.node.lab,
                            textsize = input$textsize,
                            cat = input$cat,
                            first.chars = input$first.chars,
                            field = input$field,
                            delim = input$delim,
                            cat_file = input$cat_file$datapath)
        })
        
        ### dynamic UI outputs
        output$legendpos <- renderUI({
                if (input$legend != "none") {
                        selectInput("legendpos", "Legend position",
                                    choices = c("bottomright", "bottom", "bottomleft", "left",
                                                "topleft", "top", "topright", "right", "center"),
                                    selected = "right")
                }
        })
        
        output$cat_fc <- renderUI({
                if (input$cat == "cat_fc") {
                        numericInput("first.chars", "Number of characters", 1 , min = 1, step = 1)
                }
        })
        
        output$cat_field <- renderUI({
                if (input$cat == "cat_field") {
                        numericInput("field", "Field number of delimited sequence names", 1 , min = 1, step = 1)
                }
        })
        
        output$cat_delim <- renderUI({
                if (input$cat == "cat_field") {
                        textInput("delim", "Delimiter", value = "\\.")
                }
        })
        
        output$cat_table <- renderUI({
                if (input$cat == "cat_table") {
                        fileInput("cat_file", "Categories table (ID,category)", accept = ".csv")
                }
        })
        
        output$label4ut <- renderUI({
                if (input$treetype != "p" & input$label != "symbol" & input$label != "none") {
                        radioButtons("label4ut", "Label direction",
                                     choices = c("axial", "horizontal"),
                                     inline = TRUE)
                }
        })
        
        output$select_outgroup <- renderUI({
                selectInput("select_outgroup", "Select outgroup",
                            choices = c("none", unrooted_tree()$tip.label),
                            selected = "none")
        })
        
        # observeEvent(input$sample_data, {
        #         # msa_in <- reactive({
        #         #         seqinr::read.alignment(file = "data/test_msa.fasta", format = "fasta")
        #         # })
        #         
        #         session$sendCustomMessage(type = "resetFileInputHandler", "data/test_msa.fasta")
        #         
        # })
        #         

        ### Output plot
        output$rainbowTreePlot <- renderPlot({
                rainbowtreeplot()
        }, height = function() {
                session$clientData$output_rainbowTreePlot_width * 0.75
        })
        
        ### Download
        output$plotDownload <- downloadHandler(
                filename = "rainbowtree.pdf",
                content <- function(file) {
                        pdf(file)
                        if (input$select_outgroup == "none") {
                                tmptree = unrooted_tree()
                        } else {
                                tmptree = rooted_tree()
                        }
                        rainbowtree(tmptree,
                                    treetype = input$treetype,
                                    treetitle = input$treetitle,
                                    label = input$label,
                                    label4ut = input$label4ut,
                                    legend = input$legend,
                                    legendpos = input$legendpos,
                                    branchwidth = input$branchwidth,
                                    outgroup = input$select_outgroup,
                                    color.int.edges = input$color.int.edges,
                                    show.node.lab = input$show.node.lab,
                                    textsize = input$textsize,
                                    cat = input$cat,
                                    first.chars = input$first.chars,
                                    field = input$field,
                                    delim = input$delim,
                                    cat_file = input$cat_file$datapath)
                        dev.off()
                }
        )
        
        
        output$fileDownload <- downloadHandler(
                filename = "newick_tree.nwy",
                content <- function(file) {
                        if (input$select_outgroup == "none") {
                                write.tree(unrooted_tree(), file)
                        } else {
                                write.tree(rooted_tree(), file)
                        }
                }
        )
        
        
}

### Run the application
shinyApp(ui = ui, server = server)
