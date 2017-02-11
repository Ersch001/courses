loadZetaResults <- function(ZetaResultFile)
{
  rawData <- read.table(ZetaResultFile,sep = "\t",  header = TRUE)
  colnames(rawData)[2] <- 'SampleName'
  rawData$Show.Signal <- NULL
  rawData <- separate(rawData,"SampleName",c("MeasNumber","SampleName")," ")
  
  returnData <- data.frame(rawData$SampleName)
  colnames(returnData)[1] <- 'SampleName'
  returnData$AvZetaPotential_mV <- rawData$P.ave...mV
  returnData$AvConductivity_Scm <- rawData$K.ave...S.cm
  
  return(returnData)
}


#' Loads all text files from the folder, assuming that these are exported as ASCII single file per samples 
#' directly from the stabino software. It will return the average values, summary statistics and distributions for these 
#' samples in a structured element
loadStabinoExportMultiple <- function(dataFolder)
{
  library(reshape2)
  StabinoSizeFiles <- list.files(dataFolder,pattern = "\\.txt$", full.names = TRUE)
  SampleInformation <- data.frame()
  SummaryData <- data.frame()
  Peaks <- data.frame()
  
  Distributions <- data.frame()
  
  for(i in 1:length(StabinoSizeFiles))
  {
    this <- loadStabinoExportSingle(StabinoSizeFiles[i])
    
    thisSummary <- melt(this$SummaryData)
    colnames(thisSummary) <- c("value","variable")
    thisSummary$SampleName <- rep(this$SampleInformation$SampleName,length(thisSummary[,1]))
    thisSummary$MeasDate <- rep(this$SampleInformation$MeasDate,length(thisSummary[,1]))
    SummaryData <- merge(SummaryData,thisSummary,all=TRUE)
    
    thisDistribution <- this$Distribution
    thisDistribution$SampleName <- rep(this$SampleInformation$SampleName,length(thisDistribution[,1]))
    thisDistribution$MeasDate <- rep(this$SampleInformation$MeasDate,length(thisDistribution[,1]))
    Distributions <- merge(Distributions,thisDistribution,all = TRUE)
  }
  SummaryData$MeasDate <- as.POSIXct(SummaryData$MeasDate)
  Distributions$MeasDate <- as.POSIXct(Distributions$MeasDate)
  
  return(list(SummaryData = SummaryData,Distributions = Distributions))
}


loadStabinoExportSingle <- function(StabinoSizeExportFile)
{
  con  <- file(StabinoSizeExportFile, open = "r")
  
  SampleInformation <- list()
  SummaryData <- list()
  Peaks <- data.frame()
  Distribution <- data.frame()
  Date <- ""
  InSummaryData = FALSE # set to true while reading the summary
  #InPeaksData = FALSE # set to true while reading the peaks
  #peaksrowstart <- 0
  InDistributionData = FALSE # set to true while reading the peaks
  distrowstart <- 0

  rownumber <- 1
  while (length(oneLine <- readLines(con, n = 1, warn = FALSE)) > 0) {
    ColNameSplit <- (strsplit(oneLine, "\\,"))
    
    
    if(gsub(" ","",ColNameSplit[[1]][1]) == "ID1:")
    {
      SampleInformation <- c(SampleInformation,SampleName = gsub(" ","",ColNameSplit[[1]][2]))
    } else if(gsub(" ","",ColNameSplit[[1]][1]) == "Date:")
    {
      Date <- gsub(" ","",ColNameSplit[[1]][2])
    } else if(gsub(" ","",ColNameSplit[[1]][1]) == "Time:")
    {
      SampleInformation <- c(SampleInformation,MeasDate = as.character(as.POSIXlt(paste(Date,gsub(" ","",ColNameSplit[[1]][2])),format = "%d/%m/%Y %H:%M")))
    }else if(gsub(" ","",ColNameSplit[[1]][1]) == "SummaryData")
    {
      # from here onwards I will collect the data
      InSummaryData <- TRUE
    }else if(gsub(" ","",ColNameSplit[[1]][1]) == "UserDefinedCalculations")
    {
      # this is the nex section so when I reach this I need to stop collecting the data for the summary
      InSummaryData <- FALSE
    }else if(InSummaryData == TRUE)
    {
      SummaryData <- c(SummaryData, as.numeric(ColNameSplit[[1]][3]) )
      # name the last element in the list according to the variable name
      names(SummaryData)[length(SummaryData)] <- ColNameSplit[[1]][2]
    }else if(gsub(" ","",ColNameSplit[[1]][1]) == "Size(nm)")
    {
      # from here onwards I will collect the data
      InDistributionData <- TRUE
      distrowstart <- rownumber -1
    }else if(InDistributionData == TRUE & gsub(" ","",ColNameSplit[[1]][1]) == "1.13") # this assumes that the last row of the distribution has the value 1.13
    {
      # this is the nex section so when I reach this I need to stop collecting the data for the summary
      InDistributionData <- FALSE
      distrows <- rownumber - distrowstart
      Distribution <- read.table(StabinoSizeExportFile,skip = distrowstart,sep = ",", nrows = distrows,header = TRUE)
      colnames(Distribution) <- gsub("X","",colnames(Distribution))
      colnames(Distribution) <- gsub("^\\.","",colnames(Distribution))
      colnames(Distribution) <- gsub("\\.$","",colnames(Distribution))
      colnames(Distribution) <- gsub("^\\.\\.","",colnames(Distribution))
      colnames(Distribution) <- gsub("\\.","_",colnames(Distribution))
      colnames(Distribution) <- gsub("Chan","Dist",colnames(Distribution))
    }

    rownumber <- rownumber +1
  } 
  
  close(con)
  
  return(list(SampleInformation = SampleInformation,SummaryData = SummaryData,Peaks = Peaks,Distribution = Distribution))
}