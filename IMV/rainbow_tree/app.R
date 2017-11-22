library(shiny)
library(dplyr)
library(ape)
library(seqinr)
source('rainbowtree.r')

unrootedNJtree <- function(alignment, type)
{
  # this function requires the ape and seqinR packages:
  # define a function for making a tree:
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
    mytree <- makeLabel(mytree, space="") # get rid of spaces in tip names.
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



rootedNJtree <- function(alignment, theoutgroup, type){
  # load the ape and seqinR packages:
  library(seqinr)
  # define a function for making a tree:
  makemytree <- function(alignmentmat, outgroup=`theoutgroup`){
    alignment <- ape::as.alignment(alignmentmat)
    if (type == "protein"){
      mydist <- dist.alignment(alignment)
    }
    else if (type == "DNA"){
      alignmentbin <- as.DNAbin(alignment)
      mydist <- dist.dna(alignmentbin)
    }
    mytree <- nj(mydist)
    mytree <- makeLabel(mytree, space="") # get rid of spaces in tip names.
    myrootedtree <- root(mytree, outgroup, r=TRUE)
    return(myrootedtree)
  }
  # infer a tree
  mymat  <- as.matrix.alignment(alignment)
  myrootedtree <- makemytree(mymat, outgroup=theoutgroup)
  # bootstrap the tree
  myboot <- boot.phylo(myrootedtree, mymat, makemytree)
  # plot the tree:
  # plot.phylo(myrootedtree, type="p")  # plot the rooted phylogenetic tree
  # nodelabels(myboot,cex=0.7)          # plot the bootstrap values
  mytree$node.label <- myboot   # make the bootstrap values be the node labels
  return(mytree)
}



### UI
ui <- fluidPage(
  
  ### Application title
  titlePanel("Rainbow Tree"),
  
  ### Sidebar panel
  sidebarLayout(
    sidebarPanel(
      fileInput("msaFile", "Upload multiple sequence alignment",
                accept = c("fasta", "fna", "fas")),
      textInput("treetitle", "Tree title"),
      radioButtons("treetype", "Tree Type",
                   choiceNames = c("phylogram", "unrooted", "fan"),
                   choiceValues = c("p", "u", "fan")),
      radioButtons("label", "Label",
                   choices = c("seqname", "category", "symbol", "none"),
                   selected = "seqname"),
      hr(),
      radioButtons("color", "Color",
                   choiceNames = c("first character(s)", "column"),
                   choiceValues = c("color_fc", "color_field")),
      uiOutput("color_fc"),
      uiOutput("color_field"),
      uiOutput("color_delim"),
      hr(),
      # radioButtons("label4ut", "label4ut",
      #              choices = c("axial", "horizontal"),
      #              selected = "axial"),
      # hr(),
      radioButtons("legend", "Legend",
                   choices = c("vertical", "horizontal", "none"),
                   selected = "vertical"),
      uiOutput("legendpos"),
      hr(),
      sliderInput("branchwidth", "Branch width", 1, 10, 1, 1, ticks = FALSE),
      sliderInput("textsize", "Text size", 1, 5, 1, .25, ticks = FALSE),
      checkboxInput("color.int.edges", "Color internal edges"),
      checkboxInput("show.node.lab", "Show node labels"),
      width = 3
    ),
    
    ### Output
    mainPanel(
      plotOutput("rainbowTreePlot", height = 800),
      downloadButton("download", label = "Download image")
    )
  )
)






### Server
server <- function(input, output) {
  
  msa_in <- reactive({
    req(input$msaFile)
    seqinr::read.alignment(file = input$msaFile$datapath, format = "fasta")
  })
  
  unrooted_tree <- reactive({
    # no req here because input$upload already required in msa_in
    msa_in() %>%
      unrootedNJtree(type = "DNA")
  })
  
  output$unrootedDownload <- downloadHandler(
    filename = function(){
      paste0("newick_unrooted-", Sys.Date(), ".nwy")
    },
    content <- function(file){
      write.tree(unrooted_tree(), file)
    }
  )
  
  plot <- reactive({rainbowtree(unrooted_tree(),
                      treetype = input$treetype,
                      treetitle = input$treetitle,
                      label = input$label,
                      label4ut = input$lable4ut,
                      legend = input$legend,
                      legendpos = input$legendpos,
                      branchwidth = input$branchwidth,
                      color.int.edges = input$color.int.edges,
                      show.node.lab = input$show.node.lab,
                      textsize = input$textsize,
                      color = input$color,
                      first.chars = input$first.chars,
                      field = input$field,
                      delim = input$delim)
    })
  
  ### dynamic UI outputs
  output$legendpos <- renderUI({
    if (input$legend != "none") {
      selectInput("legendpos", "Legend position",
                choices = c("bottomright", "bottom", "bottomleft", "left",
                          "topleft", "top", "topright", "right", "center"),
                selected = "bottomright")
    }
  })
  
  output$color_fc <- renderUI({
    if (input$color == "color_fc") {
    numericInput("first.chars", "First n characters", 1 , min = 1, step = 1)
    }
  })
  
  output$color_field <- renderUI({
    if (input$color == "color_field") {
        numericInput("field", "Column in field n", 1 , min = 1, step = 1)
    }
  })
  
  output$color_delim <- renderUI({
    if (input$color == "color_field") {
      textInput("delim", "Delimiter",value = "_")
    }
  })
  
  ### Plot
  output$rainbowTreePlot <- renderPlot(plot())
  
  ### Download
  output$download <- downloadHandler(
    filename = "tree.png",
    content <- function(file) {
      png(file)
      plot()
      dev.off()
    })
  

}

### Run the application
shinyApp(ui = ui, server = server)
