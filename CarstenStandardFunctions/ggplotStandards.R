
##. Function to be added to the end of the GGplot functions to standardize the output and ensure that my graphs are branded
##. 
ggplotCE <- function()
{
  source('C:/Users/CARER/OneDrive - Arla Foods amba/CE_RStandardFunctions/ggplotStandards.R')
  require(ggplot2)
  require(png);
  require(grid);
  img <- readPNG(source = "C:/Users/CARER/OneDrive - Arla Foods amba/CE_RStandardFunctions/CE_Logo.PNG")
  # add rater      
  g <- rasterGrob(img, interpolate=TRUE);
  # Basic plot
  annotation_custom(g, xmin = -Inf, xmax = (Inf*0.1), ymin = -Inf, ymax = Inf)
  
  # for later there should be a easier way using + labs(caption="CarEr FPF") but this seems to be for a later version of ggplot2
}


# Multiple plot function (I found online)
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
    
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}