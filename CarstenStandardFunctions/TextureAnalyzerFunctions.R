ImportTextureAnalyzerRawData <- function(dataFolder)
{
  RawTextureDataFiles <- list.files(dataFolder,full.names = TRUE)
  returnData <- data.frame()
  for(i in 1:length(RawTextureDataFiles))
  {
    SampleName <- read.csv(RawTextureDataFiles[i], nrows = 1, row.names = NULL, header = FALSE)
    SampleName <- strsplit(as.character(SampleName[,1]),";")[[1]]
    TASamples = NULL
    index <- 1
    for(j in seq(1,length(SampleName),3))
    {
      TASamples[index] <- SampleName[j]
      index <- index +1
    }
    RawRawData <- read.csv2(RawTextureDataFiles[i], skip = 1, row.names = NULL, header = TRUE)
    
    for(j in 1:length(TASamples))
    {
      rawData = data.frame()
      rawData = RawRawData[,seq(j*3-2,j*3)]
      colnames(rawData) <- c("Force_g","Distance_mm","Time_sec")
      rawData$SampleName <- rep(TASamples[j],length(rawData[,1]))
      returnData <- rbind(returnData,rawData)
    }
  }
  return(returnData)
}