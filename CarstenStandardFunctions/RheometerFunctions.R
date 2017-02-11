


## --------------------------- load data -------------------------
#` Loads the data from an excel file and returns the data as a data frame
#` @RheometerExportFileExcel the url of the file
#` @SheetIndexes The index (starting from 1) which sheets should be imported, the equipment typically puts one measurement step in each sheet
loadTADataFromFile <- function(RheometerExportFileExcel, SheetIndexes)
{
  library("readxl");
  sampleName <- "NULL"
  measurementDate <- "NULL"
  thisDetail <- read_excel(RheometerExportFileExcel,sheet = 1)
  for(i in 1:length(thisDetail$Filename))
  {
    if(is.na(thisDetail[i,1]))
    {
      
    } else if(thisDetail[i,1] == "Sample name")
    {
      sampleName <- as.character(thisDetail[[i,2]])
    } else if(thisDetail[i,1] == "rundate")
    {
      if(grepl("-",thisDetail[[i,2]]))
      {
        measurementDate <- as.Date(thisDetail[[i,2]],format = "%d-%m-%Y")
      }
      else
      {
        measurementDate <- as.Date(as.character(as.Date(as.numeric(thisDetail[[i,2]]),origin = "1899-12-30")),format = "%Y-%d-%m")
      }
        
    }
  }
  combinedSheet <- NULL
  for(s in 1:length(SheetIndexes))
  {
    thisSheet <- read_excel(RheometerExportFileExcel,sheet = SheetIndexes[s])
    colnames(thisSheet) <- NULL
    colnames(thisSheet) <- gsub(" ", "", thisSheet[1,],fixed = TRUE)
    thisSheet <- thisSheet[-1,]
    thisSheet <- thisSheet[-1,]
    for(i in 1:length(thisSheet))
    {
      thisSheet[,i] <- as.double(as.numeric(as.matrix(thisSheet[,i])))
    }
    thisSheet$SampleName = rep(sampleName,length(thisSheet[,1]))
    thisSheet$MeasurementDate = rep(measurementDate,length(thisSheet[,1]))
    thisSheet$StepNumber = rep(SheetIndexes[s],length(thisSheet[,1]))
    combinedSheet <- rbind(combinedSheet,thisSheet)
  }
  return(combinedSheet);
}

#` Loads the data from the excel files as exported from the TA instruments rheometer
#` Expects all excel files to be in one folder without any additional files present
#` @RheometerExportFolder the folder with the files
#` @SheetIndexes The index (starting from 1) which sheets should be imported, the equipment typically puts one measurement step in each sheet
loadTADataFromFolder <- function(RheometerExportFolder, SheetIndexes)
{
  RawFlowDataFiles <- list.files(RheometerExportFolder,full.names = TRUE)
  returnData <- data.frame()
  usedSamples <- ""
  for(i in 1:length(RawFlowDataFiles))
  {
    thisFileOutput <- loadTADataFromFile(RawFlowDataFiles[i], SheetIndexes)
    thisFileOutput$DoubleMeasCode <- rep(length(usedSamples[usedSamples == thisFileOutput$SampleName[1]]),length(thisFileOutput[,1]))
    usedSamples <- c(usedSamples,unique(thisFileOutput$SampleName))
    library("plyr")
    returnData <- rbind.fill(returnData,thisFileOutput);
  }
  return(returnData)
}


#` This function loads the data from a text file which contains the 
#` data exported from the Anton Paar rheometer and returns a data frame.
#` Multiple samples can be in one file and will be separated in the final data frame
#` @ ExportedFile the text file containing the raw data as copied from the anton paar software
loadAntonPaarDataFromFile <- function(ExportedFile)
{
    con  <- file(ExportedFile, open = "r")
    numberOfLines <- 0
    while (length(oneLine <- readLines(con, n = 1, warn = FALSE)) > 0)
    {
        numberOfLines <- numberOfLines+1
    }
    con  <- file(ExportedFile, open = "r")
    returnData <- data.frame()
    savedSamples <- ""
    thisSampleName <- ""
    thisMeasDate <- ""
    numDataPointsNextChunk <- 0
    rownumber <- 1
    thisData <- data.frame()
    StepNumber <- 1
    while (length(oneLine <- readLines(con, n = 1, warn = FALSE)) > 0)
    {
        ColNameSplit <- (strsplit(oneLine, "\t"))
        if(length(ColNameSplit[[1]])<1)
        {
            1+1
        }else if(ColNameSplit[[1]][1] == "Name:")
        {
            thisSampleName <- ColNameSplit[[1]][4]
        } else if(ColNameSplit[[1]][1] == "Measuring Date/Time:")
        {
            thisMeasDate <- ColNameSplit[[1]][4]
        } else if(ColNameSplit[[1]][1] == "Number of Data Points:")
        {
            numDataPointsNextChunk <- as.numeric(ColNameSplit[[1]][4])
        } else if(ColNameSplit[[1]][1] == "Meas. Pts.")
        {
            thisthisData <- read.table(ExportedFile,skip = rownumber+1,sep = "\t", nrows = numDataPointsNextChunk,header = FALSE)
            colnames(thisthisData) <- ColNameSplit[[1]]
            thisthisData$StepNumber <- rep(StepNumber,length(thisthisData[,1]))
            if(length(thisData) >1)
                thisData <- rbind(thisData, thisthisData)
            else
                thisData <-  thisthisData
            rm(thisthisData)
            StepNumber <- StepNumber + 1
        }else if(ColNameSplit[[1]][1] == "Data Series Information" | rownumber >= numberOfLines)
        {
            if( length(thisData)>1)
            {
                thisData$SampleName <- rep(thisSampleName,length(thisData[,1]))
                thisData$MeasurementDate <- rep(thisMeasDate,length(thisData[,1]))
                thisData$DoubleMeasCode <- rep(length(savedSamples[savedSamples == thisSampleName]),length(thisData[,1]))
                if(length(returnData) == 0)
                    returnData <- thisData
                else
                    returnData <- merge(returnData,thisData,all = TRUE)
                thisData <- data.frame()
                thisMeasDate <- ""
                thisSampleName <- ""
                savedSamples <- c(savedSamples,thisSampleName)
            }
            StepNumber <- 1
        }
        rownumber <- rownumber +1
    }
    for(i in 1:11)
    {
        returnData[,i] <- as.numeric(gsub(",",".",returnData[,i]))
    }
        
    colnames(returnData) <- gsub("\\.","",colnames(returnData))
    colnames(returnData) <- gsub(" ","",colnames(returnData))
    return(returnData)
}


## ------------------- Analyse Data ------------------------


