library(shiny)
library(dplyr)
library(ape)
library(seqinr)
source('rainbowtree.R')


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



# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Rainbow Tree"),
   
   # Sidebar with a slider input for number of bins 
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
        radioButtons("label4ut", "label4ut",
                     choices = c("axial", "horizontal"),
                     selected = "axial"),
        radioButtons("legend", "Legend",
                     choices = c("vertical", "horizontal", "none"),
                     selected = "vertical"),
        selectInput("legendpos", "Legend position",
                     choices = c("bottomright", "bottom", "bottomleft", "left",
                                 "topleft", "top", "topright", "right", "center"),
                     selected = "bottomright"),
        sliderInput("branchwidth", "Branch width", 0, 10, 1, 1, ticks = FALSE),
        sliderInput("textsize", "Text size", 0, 10, 1, 1, ticks = FALSE),
        radioButtons("color.int.edges", "color.int.edges",
                     choices = c(TRUE, FALSE),
                     selected = TRUE),
        radioButtons("show.node.lab", "Show node labels",
                     choices = c(TRUE, FALSE),
                     selected = TRUE),
        width = 3
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("rainbowTreePlot", height = 800),
        downloadButton("unrootedDownload", label = "Download unrooted tree in Newick format", class = NULL)
      )
   )
)




### Define server logic required to draw a histogram
server <- function(input, output) {

  msa_in <- reactive({
    # require input to be specified
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
   
  output$rainbowTreePlot <- renderPlot(rainbowtree(unrooted_tree(),
                                                   treetype = input$treetype,
                                                   treetitle = input$treetitle,
                                                   label = input$label,
                                                   label4ut = input$lable4ut,
                                                   legend = input$legend,
                                                   legendpos = input$legendpos,
                                                   branchwidth = input$branchwidth,
                                                   color.int.edges = input$color.int.edges,
                                                   show.node.lab = input$show.node.lab))
}


### Run the application 
shinyApp(ui = ui, server = server)
