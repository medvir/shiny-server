library(shiny)
library(dplyr)
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


rootedNJtree <- function(alignment, theoutgroup, type) {
  # this function requires the ape and seqinR packages
  # define a function for making a tree
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


#-#-#-#-#-#-# UI #-#-#-#-#-#-#
ui <- fluidPage(
  
  ### Title panel
  titlePanel("Rainbow Tree"),
  
  ### Sidebar panel
  sidebarLayout(
    sidebarPanel(
      fileInput("msaFile", "Upload multiple sequence alignment",
                accept = c("fasta", "fna", "fas")),
      #textInput("treetitle", "Tree title"), ### not using tree title at the moment
      radioButtons("treetype", "Tree Type",
                   choiceNames = c("phylogram", "unrooted", "fan"),
                   choiceValues = c("p", "u", "fan")),
      radioButtons("label", "Label",
                   choices = c("seqname", "category", "none"), #"symbol", ### not using symbol at the moment
                   selected = "seqname"),
      uiOutput("label4ut"),
      hr(),
      sliderInput("branchwidth", "Branch width", 1, 10, 2, 1, ticks = FALSE),
      sliderInput("textsize", "Text size", 1, 5, 1.5, .25, ticks = FALSE),
      hr(),
      radioButtons("color", "Coloring",
                   choiceNames = c("by first character(s)", "by column/field"),
                   choiceValues = c("color_fc", "color_field")),
      uiOutput("color_fc"),
      uiOutput("color_field"),
      uiOutput("color_delim"),
      hr(),
      radioButtons("legend", "Legend",
                   choices = c("none", "vertical", "horizontal"),
                   selected = "none"),
      uiOutput("legendpos"),
      hr(),
      h5("Extras"),
      checkboxInput("show.node.lab", "Show node labels"),
      checkboxInput("color.int.edges", "Color internal edges"),
      width = 3
    ),
    
    ### Main Panel
    mainPanel(
      plotOutput("rainbowTreePlot", height = 1000),
      downloadButton("plotDownload", label = "Download rainbow plot (png)"),
      downloadButton("unrootedDownload", label = "Download unrooted tree (nwy)")
      )
  )
)


#-#-#-#-#-#-# Server #-#-#-#-#-#-#
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
  

  rainbowtreeplot <- reactive({rainbowtree(unrooted_tree(),
                      treetype = input$treetype,
                      treetitle = input$treetitle,
                      label = input$label,
                      label4ut = input$label4ut,
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
    numericInput("first.chars", "Number characters", 1 , min = 1, step = 1)
    }
  })
  
  output$color_field <- renderUI({
    if (input$color == "color_field") {
        numericInput("field", "Field number", 1 , min = 1, step = 1)
    }
  })
  
  output$color_delim <- renderUI({
    if (input$color == "color_field") {
      textInput("delim", "Delimiter", value = "\\.")
    }
  })
  
  output$label4ut <- renderUI({
    if (input$treetype != "p" & input$label != "none") {
      radioButtons("label4ut", "Label direction",
                   choices = c("axial", "horizontal"))
    }
  })
  
  
  ### Plot
  output$rainbowTreePlot <- renderPlot(rainbowtreeplot())
  
  ### Download
  output$plotDownload <- downloadHandler(
    filename = "rainbowtree.png",
    content <- function(file) {
      png(file)
      rainbowtree(unrooted_tree(),
                  treetype = input$treetype,
                  treetitle = input$treetitle,
                  label = input$label,
                  label4ut = input$label4ut,
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
      dev.off()
    })
  
  
  output$unrootedDownload <- downloadHandler(
    filename = "newick_unrooted.nwy",
    content <- function(file) {
      write.tree(unrooted_tree(), file)
    }
  )
  
}

### Run the application
shinyApp(ui = ui, server = server)
