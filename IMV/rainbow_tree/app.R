#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(dplyr)
library(ape)
source('rainbowtree.R')
# library(seqinr)


unrootedNJtree <- function(alignment, type)
{
  # this function requires the ape and seqinR packages:
  # library(ape)
  library(seqinr)
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
   titlePanel("Old Faithful Geyser Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        fileInput("msaFile", "Upload multiple sequence alignment",
                  accept = c("fasta", "fna", "fas")),
        size = 2
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        #downloadButton("unrootedDownload", label = "Download unrooted tree in Newick format", class = NULL),
        plotOutput("rainbowTreePlot", height = 800)
      )
   )
)

# Define server logic required to draw a histogram
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
   
  output$rainbowTreePlot <- renderPlot(rainbowtree(unrooted_tree(), treetype = 'u'))
}

# Run the application 
shinyApp(ui = ui, server = server)
