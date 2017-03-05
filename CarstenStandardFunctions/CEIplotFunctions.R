# a standard function to plot all variables against all variables and create a lot of scatter plots
# and boxplots interactively to have a first look at data

CE_PlotAllVariablesInteractively <- function(inputDataSet)
{
  require(iplots)

  # get the number of unique variables per column
  UniqueColumnValues <- as.data.frame(unlist(lapply(inputDataSet, function(x)length(unique(x)))))
  colnames(UniqueColumnValues) <- "Count"
  UniqueColumnValues$ColName <- rownames(UniqueColumnValues)
  rownames(UniqueColumnValues) <- NULL
  
  # make scatterplots for all variables against all other variables with more than 5 unique values
  for(i in 1:length(UniqueColumnValues[,1]))
  {
    if(UniqueColumnValues[i,]$Count>5)
    {
      for(j in i:length(UniqueColumnValues[,1]))
      {
        if(UniqueColumnValues[j,]$Count>5)
        {
          if(!(UniqueColumnValues[i,]$ColName %in% UniqueColumnValues[j,]$ColName))
          {
            iplot(inputDataSet[,UniqueColumnValues[i,]$ColName],inputDataSet[,UniqueColumnValues[j,]$ColName]) 
          }
        }
      } 
    }
  }
  
  
  
  
  #imosaic(inputDataSet)
  ibar(inputDataSet[,1])
}