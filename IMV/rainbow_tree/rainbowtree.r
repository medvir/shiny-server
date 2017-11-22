library(ape)

outputformat <- 'png'

mypalette=c(
  "#ED1C24", "#F7941D", "#FFC20E", "#8DC73F",
  "#00A651", "#008FD4", "#235192", "#662D91",
  "#C51B7D", "#F768A1", "#F1B6DA", "#8C510A",
  "#DFC27D", "#FFFFBF", "#01665E", "#80CDC1",
  "#92C5DE", "#9970AB", "#7B68EE", "#888888");



color_edge = function(edgecolors, mytree, node) {
### The purpose of this function is to color a parent edge of an interior node such that
### any edge with decendent leaves that are all the same color
### (monophyletic) gets the same color as its leaves.  If descendents
### of the node are different colors (polyphyletic), the edge is
### colored black.

    # root                   
    if (length(mytree$edge[which(mytree$edge[,2] == node), 2]) <1 ) 
       return (edgecolors)

    # already visitied node
    if (edgecolors[which(mytree$edge[,2] == node)] != 'black'  ) {
        return (edgecolors)
    }

    if (length(mytree$edge[which(mytree$edge[,1] == node),2]) > 0) {
        mycolor =  NULL
        siblings = mytree$edge[which(mytree$edge[,1] == node), 2]
        is_same_color = T

        # compare color of sibling nodes 
        if (length(siblings) > 0) {

            for (sibling in c(2:length(siblings))) {
                mycolor = edgecolors[which(mytree$edge[,2] == siblings[sibling])]
                if (edgecolors[which(mytree$edge[,2] == siblings[(sibling-1)])] != 
                    edgecolors[which(mytree$edge[,2] == siblings[sibling])]) { 
                        is_same_color = F
                }
            }
        }
        if (is_same_color) 
            # monophyletic,
            edgecolors[which(mytree$edge[,2] == node)] = mycolor  
      }
    edgecolors
}

color_interior_edges = function(edgecolors, mytree) {
    depth_first = reorder.phylo(mytree, "pruningwise")   # reorder tree$edge. depth-first traversal
    
    for (i in c(1:length(mytree$edge[,1]))) {
        edge=depth_first$edge[i,]
        edgecolors = color_edge(edgecolors, mytree, edge[1])
    }
    edgecolors
}

rainbowtree <- function ( 
                mytree,                     # the tree
                tablefile,                  # the lookup table file name 
                                            #   a 3- or 5-column table. Sequence name (1st column), 
                                            #   integer index (1-20) to colors on palette (2nd), 
                                            #   and color category (3rd). The 4th column is for
                                            #   symbol ID's(0-18) and the 5th is for symbol group names.
                                            #   When label='symbol',  the 4,5th colums should be specified.
                treetype,                   # 
                treetitle,                  # tree title  
                label,                      # a string specifying the type of tip labels:
                                            #   'category', 'seqname',  'none'(no tip label)  or 'symbol'
                label4ut,                   # a string specifying the display of tip labels
                                            #   for unrooted trees: 'axial' or 'horizontal'
                legend,                     # a string specifying the legend style: 
                                            #   'vertical', 'horizontal' or 'none'(no legend)
                legendpos,                  # legend location: 'bottomright', 'bottom', ???bottomleft???, 
                                            #   ???left???, ???topleft???, ???top???, ???topright???, ???right??? or ???center'
                branchwidth,                # a positive number specifying the width of the branches
                outgroup,                   # a vector of numeric or character specifying the new outgroup
                color.int.edges,            # to propagate leaf colors upward into a tree until a mismatch is encountered 
                show.node.lab,              # to displays the node labels
                textsize,
                color,
                first.chars,
                field,
                delim) {

    ## read inputTree
    # mytree = read.tree(treefile)

    ## read inputTable
    # format of inputTable is 3 columns. 
    # first column has a sequence name that matches each leaf in the tree
    # second column is an integer used as index to mypalette (above)
    # third column(optional) is a category used as tip label or legend
    # mytable = if (label == 'symbol')
    #              read.table(tablefile, header=F, colClasses=c('character','numeric','character', 'numeric','character'))
    #          else 
    #              read.table(tablefile, header=F, colClasses=c('character','numeric','character'))
    

    
    # outgroup 
    if(!missing(outgroup)) mytree= root(mytree, outgroup, resolve.root=TRUE) 
   
    mytree = ladderize(mytree) 
    # tip colors: 
    tipcolors = rep('#000000', length(mytree$edge))

    
   if (color == "color_fc") {
     mytable <- data.frame(V1 = mytree$tip.label, V2 = 1:length(mytree$tip.label), V3 = substr(mytree$tip.label, 1, first.chars))
   } else {
     mytable <- data.frame(V1 = mytree$tip.label, V2 = 1:length(mytree$tip.label), V3 = strsplit(mytree$tip.label, delim)[[1]][field])
   }

    # iterate over leaves in the tree
    # populate values in edgecolors vector with colors 
    # replace tip.label with category if label == category
    edgecolors = rep('black', length(mytree$edge[,1])) 

    for (myi in c(1:length(mytree$tip.label))) {
      seqid = mytree$tip.label[myi]
      myinx = mytable$V2[which(mytable$V1 == seqid)]   # palette index
      
      # ensure that myinx is not null! as error check
      if (length(myinx) == 0)  stop('No color code for ', seqid, 'in *.txt')
      
      if (myinx > length(mypalette)) myinx = length(mypalette)
      
      # assign edge colors to color elements of mypalette vector
      edgecolors[which(mytree$edge[,2] == myi)] = mypalette[myinx]
      
      if (label == 'category')
        mytree$tip.label[myi]= paste(mytable$V3[c(which(mytable$V1 == seqid))])   # replace seqid with a category name
    }
    
    # legend
    legenditems=vector()
    legendcolors=vector()
    if (legend != 'none') {
        pIndex = array(mytable$V2);
        pIndex[pIndex > length(mypalette)] = length(mypalette)
        pIndexf = factor(pIndex)
        legendvalues = tapply(array(mytable$V3), pIndexf, unique)
        inx_uniq = attr(legendvalues, "names")   # 'names' hold palette indices
        
        for (t in 1:length(inx_uniq)) {
            pindex = inx_uniq[t]
            legendcolors[t] = mypalette[as.numeric(pindex)]
            legenditems[t] = paste(sort(legendvalues[[pindex]]), collapse = ' ') # could be many itmes per color
       }
    }

    mypch=vector()
    mylty=vector()
    symbolcolors=vector()
    symbols=vector()

    if (label == 'symbol') {
        for (t in 1:length(legenditems)) {
            mypch[t]= NA;
            mylty[t]= 1;
       }
    
        uqSymbols = unique(mytable$V4);
        symLegenditems=vector()
        for (t in 1:length(uqSymbols)) {
            symLegenditems = c(symLegenditems, paste(mytable$V5[which(mytable$V4 == uqSymbols[t])[1]], uqSymbols[t], sep=':::'))
        }

        symLegenditems = sort(symLegenditems)

        for (t in 1:length(symLegenditems)) {
            tmp = unlist(strsplit(symLegenditems[t], ":::"))
            legenditems = c(legenditems, tmp[1])
            legendcolors = c(legendcolors,'black')
            mypch = c(mypch, as.numeric(tmp[2]))
            mylty = c(mylty, NA)

        }
        for (tip in c(1:length(mytree$tip.label))) {
            symbols[tip]= mytable$V4[which( mytable$V1 ==  mytree$tip.label[tip])]
            symbolcolors[tip]= edgecolors[which(mytree$edge[,2] == tip )]
        }
    }

    ### color interior edges - pth 03 Dec 2012
    if (color.int.edges) 
        edgecolors = color_interior_edges(edgecolors, mytree)
  
    ## prepare to write output
    showtiplabel = if (label == 'none' ) F else T
    
    # set margins
    #par(oma=c(0,0,1.5,0))
    #par(mai=c(0,0,0,0), adj=0)
    
    # these influence how line ends are drawn and joined
    par(ljoin=1, lend=1)
    
    # this call to plot() is the main drawing method!
    cexvalue = if (label == 'category' ) 1 else ( if (treetype == 'fan') 0.5 else 0.75)
    
    if (label != 'symbol') {
      plot(mytree, show.node.label = show.node.lab, edge.width=branchwidth,
           edge.col = edgecolors, show.tip.label = showtiplabel, no.margin = T,
           type = treetype, font = 1.5, lab4ut = label4ut, label.offset = 0.001,
           cex = cexvalue * textsize, tip.color = tipcolors) 
    } else {
      plot(mytree, show.node.label = show.node.lab, edge.width=branchwidth,
           edge.col = edgecolors, show.tip.label = F, no.margin = T, type = treetype) 
      tiplabels(pch = symbols, col = symbolcolors, lwd = branchwidth) 
    }
    
    # title                    
    if(!missing(treetitle))  {
      par(font.main = 2, cex = 1.5)
      title(main = treetitle, outer = FALSE);
    }
    
    # legend    
    if (legend != 'none' ) {
      if (label != 'symbol') {
        legend(legendpos, horiz = (legend == 'horizontal'), legenditems,
               cex = 0.5 * textsize, bty='o', lwd = 2, col = legendcolors ) 
      } else {
        legend(legendpos, horiz = (legend == 'horizontal'), legenditems,
               cex = 0.5 * textsize, bty='o', lwd = 1, col = legendcolors,
               pch = mypch, lty = mylty ) 
      }
    }
    
    # scale bar
    add.scale.bar(cex = 0.5 * textsize)

}
