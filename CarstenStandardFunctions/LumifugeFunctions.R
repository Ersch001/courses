
## ------- Import data ---------------------------


#` this function loads the raw Lumifuge data and then saves it into a .R file
#` using dput() which will be located in the same location as the raw data.
#` In this way the data can be loaded easily again using the dget()
#` function and does not have to be transformed again.
#` @param DataFolder The folder where the original text files as exported from the lumifuge are located
#` @param FileName The name of the file without the .R extension where the data will be written to
loadTransformAndSaveRawLumifugeData <- function(DataFolder,FileName)
{
  InternalFileName <- paste0(DataFolder,FileName,".RData")
  rawLumifugeData <- loadRawLumifugeProfiles(DataFolder)
  save(rawLumifugeData,file = InternalFileName)
  rm(rawLumifugeData)
}

#` This function simply loads the raw Luifuge data from the .R Dump as created
#` using the loadTransformAndSaveRawLumifugeData() function.
#` @param DataFolder The folder where the original text files as exported from the lumifuge are located
#` @param FileName The name of the file without the .R extension from where the data should be loaded
loadRawLumifugeDataFromDumpedRFile <- function(DataFolder,FileName)
{
  InternalFileName <- paste0(DataFolder,FileName,".RData")
  load(InternalFileName)
  return(rawLumifugeData)
  
}


#` This function loads all the text files from a selected folder
#` The files are assumed to be the raw profiles as exported from the 
#` Lumifuge equipment
#' @param dataFolder The path of the data folder containing the text files
loadRawLumifugeProfiles <- function(dataFolder, decimal = ",")
{ 
  require(foreach)
  require(doSNOW)
  LumifugeProfileFiles <- list.files(dataFolder,pattern = "\\.txt$", full.names = TRUE)
  returnData = data.frame()
  usedSamples <- ""
  for(i in 1:length(LumifugeProfileFiles))
  {
    # this is to get the class types in the first 
    Initial <- read.table(LumifugeProfileFiles[i], skip = 11, nrows = 10, row.names = NULL,sep="\t",  header = FALSE, dec = decimal)
    classes <- sapply(Initial,class)
    RawRawData <- read.table(LumifugeProfileFiles[i], skip = 10, row.names = NULL, header = FALSE,sep="\t",  quote = "",colClasses = classes, dec = decimal)
    
    SampleName <- getSampleNameLum(LumifugeProfileFiles[i])
    SampleDate <- getSampleDateLum(LumifugeProfileFiles[i])
    
    MetaData <- read.table(LumifugeProfileFiles[i], skip = 4, nrows = 6, row.names = 1,sep="\t", quote = "", header = FALSE)
    
    # I will once run this outside the for loop to ensure that the object is completely loaded and then 
    # in the for loop I only have to update the information that is actually changing which should make it faster
    
    numberCores <- getDoParWorkers() # gets the number of cores available to the system, not sure why but it seems to get one less than I actually have
    cl<-makeCluster(numberCores) #the attribute sets how many cores are to be used
    registerDoSNOW(cl)
    
    InterReturnData<-foreach(j = 1:(length(RawRawData)-1),.combine=rbind) %dopar%
    {
      rawData = data.frame(RawRawData[,1])
      rawData[,2] = RawRawData[j+1]
      colnames(rawData) <- "Distance"
      colnames(rawData)[2] <- "Transmission"
      rawData$RPM = rep(as.numeric(gsub(",",".",as.character(MetaData[2,j]))),length(rawData[,1]))
      rawData$SampleName = rep(SampleName,length(rawData[,1]))
      rawData$DateTime = rep(SampleDate,length(rawData[,1]))
      rawData$Temperature = rep(as.numeric(gsub(",",".",as.character(MetaData[1,j]))),length(rawData[,1]))
      rawData$ElapsedTimeSec = rep(as.numeric(gsub(",",".",as.character(MetaData[5,j]))),length(rawData[,1]))
      rawData
    }
    stopCluster(cl)
    InterReturnData$DoubleMeasCode <- rep(length(usedSamples[usedSamples == SampleName]),length(InterReturnData[,1]))
    usedSamples <- c(usedSamples,SampleName)
    returnData <- rbind(returnData,InterReturnData)
    rm(InterReturnData)
  }
  returnData$UniqueSampleID <- paste0(returnData$SampleName,"-",as.character(sprintf("%04d",as.numeric(returnData$DoubleMeasCode))))
  returnData$Distance <- as.numeric(as.character(returnData$Distance))
  returnData$Transmission <- as.numeric(as.character(returnData$Transmission))
  returnData$DateTime <- as.POSIXct(returnData$DateTime)
  return(returnData)
}

# to load the table where the integration over time was exported from the lumifuge
loadIntegralData <- function(LumifugeIntegrationFile)
{
  
  RawRawData <- read.xlsx(LumifugeIntegrationFile,1, startRow = 6, header = FALSE)
  
  SampleName <- read.xlsx(LumifugeIntegrationFile,1,startRow = 3,endRow = 3, header = FALSE)
  SampleName <- SampleName[!is.na(SampleName)]
  SampleNames = NULL
  index = 1
  for(i in 1:length(SampleName))
  {
    SampleNames[i] = as.character(unlist(SampleName[i]))
  }
  returnData <- data.frame()
  usedSampleNames <- ""
  for(i in 1:length(SampleNames))
  {
    rawData = data.frame()
    rawData = RawRawData[,seq(i*4-3,i*4)]
    rawData <- rawData[complete.cases(rawData),]
    colnames(rawData) <- c("TimeSec","Integral","TempC","RPM")
    rawData$SampleName <- rep(SampleNames[i],length(rawData[,1]))
    thisTable <- table(usedSampleNames)
    if(SampleNames[i] %in% usedSampleNames)
      rawData$DoubleMeasCode <- rep(as.numeric(thisTable[names(thisTable)==SampleNames[i]]),length(rawData[,1]))
    else
      rawData$DoubleMeasCode <- rep(0,length(rawData[,1]))
    returnData <- rbind(returnData,rawData)
    usedSampleNames <- c(usedSampleNames,SampleNames[i])
  }
  rm(rawData)
  returnData$UniqueSampleID <- paste0(returnData$SampleName,"-",as.character(sprintf("%04d",as.numeric(returnData$DoubleMeasCode))))
  return(returnData)
}


# to load the table where the integration over time was exported from the lumifuge
loadFrontTrackingDataFromLumifugeExport <- function(LumifugeFromtTrackingExportFile)
{
  RawRawData <- read.csv(LumifugeFromtTrackingExportFile, skip=5, header = TRUE)
  
  #SampleName <- read.xlsx(LumifugeFromtTrackingExportFile,1,startRow = 3,endRow = 3, header = FALSE)
  SampleName <- read.csv(LumifugeFromtTrackingExportFile,skip = 2,nrows = 1, header = FALSE)
  SampleName <- SampleName[!is.na(SampleName)]
  #Threshold <- read.xlsx(LumifugeFromtTrackingExportFile,1,startRow = 5,endRow = 5, header = FALSE)
  Threshold <- read.csv(LumifugeFromtTrackingExportFile,skip = 4,nrows = 1, header = FALSE)
  Threshold <- ifelse(Threshold=="Threshold in %",NA,Threshold)
  Threshold <- Threshold[!is.na(Threshold)]
  #Meniscu <- read.xlsx(LumifugeFromtTrackingExportFile,1,startRow = 4,endRow = 4, header = FALSE)
  Meniscu <- read.csv(LumifugeFromtTrackingExportFile,skip = 3,nrows = 1, header = FALSE)
  Meniscu <- as.numeric(Meniscu)
  Meniscu <- ifelse(Meniscu == 1,NA,Meniscu)
  Meniscu <- Meniscu[!is.na(Meniscu)]
  SampleNames = NULL
  Thresholds <- NULL
  Meniscus <- NULL
  index = 1
  for(i in 1:length(SampleName))
  {
    SampleNames[i] = as.character(unlist(SampleName[i]))
    Thresholds[i] = as.numeric(as.character(unlist(Threshold[i])))
    Meniscus[i] = as.numeric(as.character(unlist(Meniscu[i+(i-1)])))
  }
  rm(Meniscu);rm(SampleName);rm(Threshold)
  returnData <- data.frame()
  usedSampleNames <- ""
  LastTreshhold <- 0
  for(i in 1:length(SampleNames))
  {
    rawData = data.frame()
    rawData =  RawRawData[,seq(i*5-4,i*5-1)]
    rawData <- rawData[complete.cases(rawData),]
    # if I move to a new threshold I should start all over again with my
    # count of the samples to have the right double measurement codes
    if(Thresholds[i] != LastTreshhold)
    {
      usedSampleNames <- ""
      LastTreshhold <- Thresholds[i]
    }
    
    # if there is some data in the table then save it
    if(length(rawData[,1])>0)
    {
      colnames(rawData) <- c("Time_s","Position_mm","Temp_C","RPM")
      rawData$SampleName <- rep(SampleNames[i],length(rawData[,1]))
      rawData$Threshold <- rep(Thresholds[i],length(rawData[,1]))
      rawData$Meniscus <- rep(Meniscus[i],length(rawData[,1]))
      thisTable <- table(usedSampleNames)
      if(SampleNames[i] %in% usedSampleNames)
        rawData$DoubleMeasCode <- rep(as.numeric(thisTable[names(thisTable)==SampleNames[i]]),length(rawData[,1]))
      else
        rawData$DoubleMeasCode <- rep(0,length(rawData[,1]))
      returnData <- rbind(returnData,rawData)
    }
    usedSampleNames <- c(usedSampleNames,SampleNames[i])
  }
  rm(rawData)
  returnData$UniqueSampleID <- paste0(returnData$SampleName,"-",as.character(sprintf("%04d",as.numeric(returnData$DoubleMeasCode))))
  returnData$Time_s <- as.numeric(returnData$Time_s)
  return(returnData)
}

## ------- Generate Plots-------------------------------------------------------------------

#` This function plots the raw Lumifuge profiles in an efficient way so that not all data is loaded into the 
#` graphics layers of ggplot.
#` @param LumifugeData The data.frame containing the raw lumifuge data in a melted way (this can be done using the function loadTransformAndSaveRawLumifugeData())
#` @param numProfiles The number of profiles (lines) to be plotted, this limits how big and crowded the graph will be
plotProfilesStackedGraph <- function(LumifugeData,numProfiles, xlim = c(0,140),RPMLimit = c(0,Inf))
{
  library(ggplot2)
  source('C:/Users/CARER/OneDrive - Arla Foods amba/CE_RStandardFunctions/ggplotStandards.R')
  LumifugeData$RPM <- round(LumifugeData$RPM/1000,digits = 1)*1000
  timeElapsed <- unique(round(LumifugeData$ElapsedTimeSec))
  profileChoice <- rep(c(TRUE,rep(FALSE,round(length(timeElapsed)/numProfiles)-1)),length(timeElapsed)/round(length(timeElapsed)/numProfiles))
  
  if(length(timeElapsed)>length(profileChoice)) ## make sure the vectors have the same length
  {
    profileChoice <- append(profileChoice,c(rep(FALSE,length(timeElapsed)-length(profileChoice)-1),TRUE),length(profileChoice))
  } else if(length(timeElapsed) < length(profileChoice))
  {
    length(profileChoice) <- length(timeElapsed)
  }
  
  profileChoice <- timeElapsed[profileChoice]
  
  mySubset <- subset(LumifugeData,round(ElapsedTimeSec) %in% (profileChoice) & RPM >= RPMLimit[1] & RPM <= RPMLimit[2])
  gradientScale <- scales::seq_gradient_pal("blue", "red", "Lab")(seq(0,1,length.out=length(unique(mySubset$ElapsedTimeSec))))
  
  ggplot(mySubset, aes(x= Distance, y = Transmission)) +
    geom_line(aes(Group = ElapsedTimeSec, colour = factor(ElapsedTimeSec)))+ ggplotCE()+ theme_light() +
    theme(legend.position='none')  + scale_colour_manual(name ='scale',values = gradientScale) + 
    facet_grid(SampleName ~ RPM, labeller = label_value) +
    scale_x_continuous(limits = xlim) + scale_y_continuous(limits = c(0,100))
}



## ------- Internal stuff and calculations ----------


#` This function returns the Sample Name from the text file as exported from the Lumifuge software
#` @param MetaDataFile The path to the text file as exported from the Lumifuge Software
getSampleNameLum <- function(MetaDataFile)
{
  SampleName <- read.table(MetaDataFile, nrows = 1, row.names = NULL, header = FALSE)
  return(as.character(SampleName[,2]))
}


#` This function returns the Date and time at which the measurement was started from the text file as exported from the Lumifuge software
#` @param MetaDataFile The path to the text file as exported from the Lumifuge Software
getSampleDateLum <- function(MetaDataFile)
{
  SampleDate <- read.table(MetaDataFile, skip = 2, nrows = 1, row.names = NULL, header = FALSE)
  return(strptime( paste(as.character(SampleDate[,3]),as.character(SampleDate[,4])), format = '%d-%m-%Y %H:%M'))
}

RPMtoRCF <- function(RPM,Distance_mm)
{return(round(1.12 *Distance_mm*(RPM/1000)^2,2))}


## ------- Advanced Analysis - Front Tracking -------------

#` Interpolates the Distance for a given set of transmissions and times.
#` Please note that the returned Temperature values are simply averages of the recorded values ithin the given dataset used for interpolation
#` interpolation of temperature would add additional computation time which at the moment is assumed to be not neccesary to implement
#` The RPM on the other hand are interpolated
#` In this function the `akima` package is used which started to give more and mor error messages. I therefore wrote another function which does the interpolation
#` even though the other function is much slower. Hopefully in the future the akima package will be updated and maybe that would lead
#` to a more stable version.
#` @params Data A Data frame containing the raw Lumifuge Data. Double measurements and different samples are supported 
#`  but no support is given for multiple RPM runs, please supply different runs of th same sample at different RPM separately
LumifugeFrontTrackingMultipleWithAkima <- function(Data, TargetTimes,  TargetTrans,  DistLimits = c(-Inf,Inf))
{
  require(akima)
  theSamples <- unique(Data$SampleName)
  theSamples <- merge(theSamples,unique(Data$DoubleMeasCode)) 
  
  mySubset <- subset(Data, Distance >= DistLimits[1] & Distance <= DistLimits[2] & ElapsedTimeSec <= max(TargetTimes) & ElapsedTimeSec >= min(TargetTimes))
  
  
  returnData <- data.frame(ElapsedTimeSec = numeric(0), Transmission= numeric(0), Distance = numeric(0))

  for(j in 1:length(theSamples[,1]))
  {
    thisSubset <- subset(mySubset, SampleName == as.character(theSamples$x[j]) & DoubleMeasCode == as.numeric(theSamples$y[j]) & Transmission >= min(TargetTrans)-0.2*min(TargetTrans) & Transmission <= max(TargetTrans) + 0.2*max(TargetTrans))
    
    if(length(unique(thisSubset$ElapsedTimeSec))>2)
    {
        if(sum(!is.na(thisSubset$Distance)) > 3 & sum(!is.na(thisSubset$Transmission)) > 3 & sum(!is.na(thisSubset$ElapsedTimeSec)) > 3)
        {
          rD <- interp( x = thisSubset$ElapsedTimeSec, y = thisSubset$Transmission, z = thisSubset$Distance, xo = TargetTimes, yo = TargetTrans, duplicate = "strip")
          rDRPM <- interp( x = thisSubset$ElapsedTimeSec, y = thisSubset$Transmission, z = thisSubset$RPM, xo = TargetTimes, yo = TargetTrans, duplicate = "strip")
          
          
          
          interReturn <- data.frame(ElapsedTimeSec = rD$x, Transmission= rD$y[1], Distance = rD$z[,1])
          interReturn$RPM <- as.numeric(rDRPM$z[,1])
          for( i in 2:length(rD$z[1,]))
          {
            interinterReturn <- data.frame(ElapsedTimeSec = rD$x, Transmission= rD$y[i], Distance = rD$z[,i])
            interinterReturn$RPM <- as.numeric(rDRPM$z[,i])
            interReturn <- rbind(interReturn,interinterReturn)
          }
          interReturn$SampleName <- rep(unique(thisSubset$SampleName),length(interReturn$ElapsedTimeSec))
          interReturn$DoubleMeasCode <- rep(unique(thisSubset$DoubleMeasCode),length(interReturn$ElapsedTimeSec))
          interReturn$DateTime <- rep(unique(thisSubset$DateTime), length(interReturn$ElapsedTimeSec))
          interReturn$Temp <- rep(mean(thisSubset$Temperature),length(interReturn$ElapsedTimeSec))
          returnData <- rbind(returnData,interReturn[complete.cases(interReturn$Distance),])
        }
    } else
    {
      ## in this case nothing happens as not enough data points are available to do an interpolation
    }
  }
  
  return(returnData)
}


#` Interpolates the Distance for a given set of transmissions and times.
#` Please note that the returned Temperature values are simply averages of the recorded values ithin the given dataset used for interpolation
#` interpolation of temperature would add additional computation time whcih at the moment does not seem to be neccesary
#` The returned RPM values on the other hand are the interpolated values.
#` This function is the slower version of an earlier function which started to give error messages based on the used package. In the future the other package should be
#` tracked for updates and maybe there will be a newer version without the bugs that have R crash at the moment when using the package (akima package)
#` 
#` The dataset requires to have a column called UniqueSampleIdentifier which is used to separate the samples
#` @params Data A Data frame containing the raw Lumifuge Data. Double measurements and different samples are supported 
#`  but no support is given for multiple RPM runs, please supply different runs of th same sample at different RPM separately
#` @params TargetTrans a vector with the transmission values at which one would like to analyze the data
#` @params TimeLimits a vector of length 2 which gives the range of times where one would like to interpolate
#` @params DistLimits a vector with length 2 which gives the range of distances over which the interpolation will occur
#` @Param TimeJumps an integer indicating how many of the existing time measurements to jump (e.g. only naalyze every 3rd would mean jump 3)
#` @Param startSearch Indicates whether to start searching for a crossing from the bottom or the top of the sample. The default is the bottom as this typically leads to a better result but in some cases it is neccesary to search from the top as well, in this case please be aware that the lower limit of distances is critical
LumifugeFrontTrackingMultiple <- function(Data, TargetTrans,TimeLimits = c(-Inf,Inf),TimeJumps = 0, DistLimits = c(-Inf,Inf),startSearch = "bottom")
{
  require(foreach)
  require(doSNOW)
  theSamples <- unique(Data$UniqueSampleID)
  mySubset <- subset(Data, Distance >= DistLimits[1] & Distance <= DistLimits[2] & ElapsedTimeSec <= TimeLimits[2] & ElapsedTimeSec >= TimeLimits[1])

  numberCores <- getDoParWorkers() # gets the number of cores available to the system, not sure why but it seems to get one less than I actually have
  cl<-makeCluster(numberCores) #the attribute sets how many cores are to be used
  registerDoSNOW(cl)
  
  returnreturnData <- foreach(j = 1:length(theSamples),.combine=rbind) %dopar%
  #for(j in 1:length(theSamples))
  {
    returnData <- data.frame()
    thisSubset <- subset(mySubset, UniqueSampleID == as.character(theSamples[j]) & Transmission >= min(TargetTrans)-0.2*min(TargetTrans) & Transmission <= max(TargetTrans) + 0.2*max(TargetTrans))
    toBeAnalyzedTimes <- rep(c(rep(FALSE,TimeJumps),TRUE),as.integer(length(unique(thisSubset$ElapsedTimeSec))/(TimeJumps+1)))
    
    if(length(unique(thisSubset$ElapsedTimeSec))>2)
    {
      for(k in unique(thisSubset$ElapsedTimeSec)[toBeAnalyzedTimes])
      {
          thisTimeSubset <- subset(thisSubset, ElapsedTimeSec == k)
          if(sum(!is.na(thisTimeSubset$Distance)) > 3 & sum(!is.na(thisTimeSubset$Transmission)) > 3 & sum(!is.na(thisTimeSubset$ElapsedTimeSec)) > 3)
          {
              tryCatch(
                  {
                      interReturn <- data.frame(Transmission= numeric(0), Distance = numeric(0),
                                                ElapsedTimeSec = numeric(0), SampleName = character(0),
                                                UniqueSampleID = character(0),DoubleMeasCode = numeric(0),
                                                DateTime = as.Date(character(0)), Temperature = numeric(0),
                                                RPM = numeric(0),CrossingNumber = numeric(0))
                      # this approach is absolutely not pretty but I dont know how to do it better at this stage
                      # I will go through all values and find the two values above and below the corresponding
                      # transmission and then interpolate the data only between those two rows
                    numCrossings = rep(1,length(TargetTrans))
                    for(i in 2:length(thisTimeSubset[,1]))
                    {
                        
                        for(thisTransi in 1:length(TargetTrans))
                              {
                              if(((thisTimeSubset$Transmission[(i-1)] - TargetTrans[thisTransi]) * (thisTimeSubset$Transmission[i] - TargetTrans[thisTransi])) <0) # check if there is a crossing with the target transmission
                              {
                                  rD <- approx( x = thisTimeSubset$Transmission[(i-1):i], y = thisTimeSubset$Distance[(i-1):i], xout = TargetTrans[thisTransi], rule = 1, ties = mean)
                                  RPM <- approx( x = thisTimeSubset$Transmission[(i-1):i], y = thisTimeSubset$RPM[(i-1):i], xout = TargetTrans[thisTransi], rule = 1, ties = mean)

                                  interReturn <- rbind(interReturn,data.frame(Transmission= rD$x, Distance = rD$y,
                                                      ElapsedTimeSec = k, SampleName = unique(thisTimeSubset$SampleName),
                                                      UniqueSampleID = theSamples[j],DoubleMeasCode = unique(thisTimeSubset$DoubleMeasCode),
                                                      DateTime = unique(thisTimeSubset$DateTime), Temperature = mean(thisTimeSubset$Temperature),
                                                      RPM = RPM$y,CrossingNumber = numCrossings[thisTransi]))
                                  numCrossings[thisTransi] = numCrossings[thisTransi] +1
                              }
                          }
                      }
                    numCrossings = rep(1,length(TargetTrans))

                      #if(startSearch == "bottom")
                      #  rD <- approx( x = rev(thisTimeSubset$Transmission), y = rev(thisTimeSubset$Distance), xout = TargetTrans, rule = 1, ties = mean)
                      #else if (startSearch == "top")
                      #    rD <- approx( x = thisTimeSubset$Transmission, y = thisTimeSubset$Distance, xout = TargetTrans, rule = 1, ties = mean)
                      #else
                      #    stop("the startPosition parameter can only have the values 'bottom' or 'top' ")
                      #interReturn <- data.frame()
                      #interReturn$ElapsedTimeSec = rep(k,length(interReturn$Transmission))
                      #interReturn$SampleName <- rep(unique(thisTimeSubset$SampleName),length(interReturn$ElapsedTimeSec))
                      #interReturn$UniqueSampleID <- rep(unique(thisTimeSubset$UniqueSampleID),length(interReturn$ElapsedTimeSec))
                      #interReturn$DoubleMeasCode <- rep(unique(thisTimeSubset$DoubleMeasCode),length(interReturn$ElapsedTimeSec))
                      #interReturn$DateTime <- rep(unique(thisTimeSubset$DateTime), length(interReturn$ElapsedTimeSec))
                      #interReturn$Temp <- rep(mean(thisTimeSubset$Temperature),length(interReturn$ElapsedTimeSec))
                      #rD <- approx( x = thisTimeSubset$Transmission, y = thisTimeSubset$RPM, xout = TargetTrans, rule = 1, ties = mean)
                      #interReturn$RPM <- rD$y
                      returnData <- rbind(returnData,interReturn)
                  },
                  finally = warning(paste("ran into catch for",thisTimeSubset$UniqueSampleID[1]))
              )
          } else
          {
              warning(paste("Not more than 3 data points in the interpolation for",thisTimeSubset$UniqueSampleID[1]))
              ## in this case nothing happens as not enough data points are available to do an interpolation
          }
      }
    } else
    {
        warning(paste("the subset did not contain enough data for sample",thisSubset$UniqueSampleID[1]))
      ## in this case nothing happens as not enough data points are available to do an interpolation
    }
    returnData
  }
  stopCluster(cl)
  return(returnreturnData)
}


LumifugeFrontTrackingGetIndexCrossings <- function(LumifugeFrontTrackingCrossings,CrossingIndex = 1)
{
  
  crossingNumberOverview <- subset(LumifugeFrontTrackingCrossings,CrossingNumber > CrossingIndex) %>%  group_by(UniqueSampleID,Transmission,ElapsedTimeSec) %>% summarise(MaxCrossingNumber = max(CrossingNumber))
  crossingNumberOverview <- merge(LumifugeFrontTrackingCrossings,crossingNumberOverview,by=c("UniqueSampleID","Transmission","ElapsedTimeSec"),all.x = TRUE)
  crossingNumberOverview[!is.na(crossingNumberOverview$MaxCrossingNumber) & crossingNumberOverview$CrossingNumber %in% CrossingIndex,]$MaxCrossingNumber <- NA
  crossingNumberOverview <- crossingNumberOverview[is.na(crossingNumberOverview$MaxCrossingNumber),]
  crossingNumberOverview$MaxCrossingNumber <- NULL
  return(subset(crossingNumberOverview,CrossingNumber %in% CrossingIndex))
}



#` This function fits a linear model to the distance over time data as extracted from the raw Lumifuge profiles
#` with the function LumifugeFrontTrackingMultiple(). It either uses a simple linear fit or a segmented linear fit and also tries to
#` estimate which points are outliers which it then removes from the fitting dataset.
#` The return value is a list with all details of the best fit.
#` @param Data The data as returned from the LumifugeFrontTrackingMultiple() function
FitLumifugeFrontTrackingResults <- function(Data)
{
  returnList <- list()
  for(thisSample in unique(Data$UniqueSampleID))
  {
    thisTransmissionData <- subset(Data,UniqueSampleID == thisSample)
    for(ThisTransmission in unique(thisTransmissionData$Transmission))
    {
    thisSubset <- subset(thisTransmissionData,Transmission == ThisTransmission)
    if(length(thisSubset[,1])>10)
    {
      ## The first fit will be a linear model where I will use the robust linear model to get rid of outliers
      library(MASS)
      lmResultRLM <- rlm(Distance~ElapsedTimeSec,thisSubset, psi = psi.hampel, init = "lts")
      
      cleanedSubset <- MyFrontTrackingDataRemoval(thisSubset,StErrEstCutOff = 0.02, maxFractionPointsRemoved = 0.3)
      if(length(cleanedSubset)>1)
      {
        lmResultRLMCleaned <- rlm(Distance~ElapsedTimeSec,data = cleanedSubset, psi = psi.hampel, init = "lts")
      }else
      {
        lmResultRLMCleaned <- list(s = Inf)
      }
 
      ## The second try will be a segmented model with two distinct regions where each region is a linear model
      ## and the cutoff between the region is determined by the fitting function as well
      library(segmented)
      
      segResultR <- list(s = Inf)
      try(segResultR <- segmented(lmResultRLM,seg.Z = ~ ElapsedTimeSec, psi = list(ElapsedTimeSec = mean(lmResultRLM$x[,2]))))
      
      
      ## from the sengmentation I obtained the cutoff point, now I will fit the data
      ## below and above this point to get a general lm result for the two parts
      if(segResultR$s != Inf & length(subset(thisSubset,ElapsedTimeSec <= segResultR$psi[1,2])[,1]) > 3 & length(subset(thisSubset,ElapsedTimeSec > segResultR$psi[1,2])[,1]) >3 )
      {
        returnSegRes <- segResultR$psi
        segFit1Res <- rlm(Distance~ElapsedTimeSec,data = subset(thisSubset,ElapsedTimeSec <= segResultR$psi[1,2]), psi = psi.hampel, init = "lts")
        segFit2Res <- rlm(Distance~ElapsedTimeSec,data = subset(thisSubset,ElapsedTimeSec > segResultR$psi[1,2]), psi = psi.hampel, init = "lts")
        segResultR <- list(SegmentationCutoff = returnSegRes, firstLinearFit = segFit1Res, secondLinearFit = segFit2Res)
      }
      else
      {
        s = list(s = Inf)
        segResultR <- list(SegmentationCutoff = Inf, firstLinearFit = s, secondLinearFit = s)
      }
      
      
      ## the idea here was to also have some segmented fit to the cleaned data
      ## but in the cleaned data the segmented idea was not implemented yet, so I will skip this
      ## in the future I should make a separate function to clean the data using a segemented fit
      ## then I could also implement this part again
      #if(length(cleanedSubset)>1 & lmResultRLMCleaned$s != Inf)
      #{
      #  try(segResultRLMCleaned <- segmented(lmResultRLMCleaned,seg.Z = ~ ElapsedTimeSec, psi = list(ElapsedTimeSec = 500)))
      #}else
      #{
      segResultRLMCleaned <- list(s = Inf) ## I left this part which will disable the segmented function to the cleaned data 
      #}
      
      
      ## 
      if(segResultRLMCleaned$s != Inf )
      {
        returnSegRes <- segResultRLMCleaned$psi
        segFit1Res <- rlm(Distance~ElapsedTimeSec,data = subset(cleanedSubset,ElapsedTimeSec <= segResultRLMCleaned$psi[1,2]), psi = psi.hampel, init = "lts")
        segFit2Res <- rlm(Distance~ElapsedTimeSec,data = subset(cleanedSubset,ElapsedTimeSec > segResultRLMCleaned$psi[1,2]), psi = psi.hampel, init = "lts")
        segResultRLMCleaned <- list(SegmentationCutoff = returnSegRes, firstLinearFit = segFit1Res, secondLinearFit = segFit2Res)
      }
      else
      {
        s = list(s = Inf)
        segResultRLMCleaned <- list(SegmentationCutoff = Inf, firstLinearFit = s, secondLinearFit = s)
      }
      
      ## some functions for debugging
      ## predictedsegResultRLMCleaned <- predict(segResultRLMCleaned)
      ## predictedlmResultRLMCleaned <- predict(lmResultRLMCleaned)
      ## plot(thisSubset$ElapsedTimeSec,thisSubset$Distance); lines(cleanedSubset$ElapsedTimeSec,cleanedSubset$Distance, type = "p", col = 2)
      ## lines(cleanedSubset$ElapsedTimeSec,predictedsegResultRLMCleaned, type = "l", col = 3) ## is green
      ## lines(cleanedSubset$ElapsedTimeSec,predictedlmResultRLMCleaned, type = "l", col = 1) ## is black
      
      
      
      ## segmentation cutoff specification
      ## because I only want to use a segmentation if really needed, I will specify that it should have at least a x fold better 
      ## fit that the linear version
      segmentationFactor <- 1.5
      sAverageSeg <- max(c(segResultR$firstLinearFit$s,segResultR$secondLinearFit$s))
      sAverageSegCleaned <- max(c(segResultRLMCleaned$firstLinearFit$s,segResultRLMCleaned$secondLinearFit$s))
      if(lmResultRLM$s <= lmResultRLMCleaned$s & lmResultRLM$s <= sAverageSeg * segmentationFactor & lmResultRLM$s <= sAverageSegCleaned * segmentationFactor )
      {
        returnList[[length(returnList)+1]] <- list(UniqueSampleID = thisSample, Transmission = ThisTransmission, analysisDetail = "rlm", AnalysisResult = lmResultRLM)
      }else if(lmResultRLMCleaned$s <= sAverageSeg * segmentationFactor & lmResultRLMCleaned$s <= sAverageSegCleaned * segmentationFactor)
      {
        returnList[[length(returnList)+1]] <- list(UniqueSampleID = thisSample, Transmission = ThisTransmission, analysisDetail = "rlm cleaned", AnalysisResult = lmResultRLMCleaned)
      }else if(sAverageSeg <= sAverageSegCleaned)
      {
        returnList[[length(returnList)+1]] <- list(UniqueSampleID = thisSample, Transmission = ThisTransmission, analysisDetail = "rlm segmented", AnalysisResult = segResultR)            
      }else
      {
        returnList[[length(returnList)+1]] <- list(UniqueSampleID = thisSample, Transmission = ThisTransmission, analysisDetail = "rlm segmented cleaned", AnalysisResult = segResultRLMCleaned)
      }
      
    }
    }
  }
  return(returnList)
}


## this will fit the data with a robust linear model, then remove the data points with the largest residuals and then fit the model again
#` for the moment I will use as a criterion that the standardErrorOfEstimates should not change more than 1% for the function to end
MyFrontTrackingDataRemoval <- function(Data, StErrEstCutOff = 0.01, maxFractionPointsRemoved = 0.2)
{
  LastStErrEst = 0.1
  StErrEstChange = 1
  maxLoopNumber = as.integer(length(Data[,1])*maxFractionPointsRemoved)
  while(StErrEstChange > StErrEstCutOff)
  {
    library(MASS)
    lmResultRLM <- rlm(Distance~ElapsedTimeSec,Data, psi = psi.hampel, init = "lts")
    Data$Residuals <- lmResultRLM$w
    Data <- Data[Data$Residuals != min(Data$Residuals),]
    
    StErrEst = lmResultRLM$s
    
    if(maxLoopNumber <= 0)
    {
      return(NA)
      break;
    }
    StErrEstChange = abs(StErrEst-LastStErrEst)/StErrEst
    LastStErrEst = StErrEst
    maxLoopNumber <- maxLoopNumber -1
    
    if(length(Data[,1])<2)
    {
      return(NA);
      break;
    }
  }
  return(Data)
}




#` This function takes the results from FitLumifugeFrontTrackingResults() and formats it into a table
#` this will allow for an easier visualization of the data
summarizeFrontTrackFitResults <- function(ResultsList)
{
  returnData <- data.frame(UniqueSampleID = character(0), Transmission = numeric(0),
                           InitialSlope = numeric(0),InitialSlopeStdev = numeric(0), InitialIntercept = numeric(0), InitialInterceptStdev = numeric(0),ElapsedTimeCutoff = numeric(0), ElapsedTimeCutoffStdev = numeric(0),
                           SecondarySlope = numeric(0),SecondarySlopeStdev = numeric(0),SecondaryIntercept = numeric(0),SecondaryInterceptStDev = numeric(0), stErrorOfEstimates = numeric(0),
                           AnalysisType = character(0))

  # loop over all the list items
  for(i in 1:length(ResultsList))
  {
    returnRow <- data.frame(UniqueSampleID = "", Transmission = NA,
                            InitialSlope = NA,InitialSlopeStdev = NA, InitialIntercept = NA, InitialInterceptStdev = NA,ElapsedTimeCutoff = NA,ElapsedTimeCutoffStdev = NA,
                            SecondarySlope = NA,SecondarySlopeStdev = NA, SecondaryIntercept = NA, SecondaryInterceptStDev = NA,stErrorOfEstimates = NA,
                            AnalysisType = NA)
    
    ## get the sample information
    thisList <- ResultsList[[i]]
    returnRow$UniqueSampleID <- thisList$UniqueSampleID
    returnRow$Transmission <- thisList$Transmission
    returnRow$AnalysisType <- thisList$analysisDetail
    
    
    # dependent on the analysis result write data into the data frame 
    if(thisList$analysisDetail == "rlm" | thisList$analysisDetail == "rlm cleaned")
    {
      thisAnalysis <- thisList$AnalysisResult
      coefficients <- summary(thisAnalysis)$coefficients
      returnRow$InitialIntercept = coefficients[1,1]
      returnRow$InitialInterceptStdev = coefficients[1,2]
      returnRow$InitialSlope = coefficients[2,1]
      returnRow$InitialSlopeStdev = coefficients[2,2]
      returnRow$stErrorOfEstimates = thisAnalysis$s
    }else if(thisList$analysisDetail == "rlm segmented" | thisList$analysisDetail == "rlm segmented cleaned")
    {
      thisAnalysis1 <- thisList$AnalysisResult$firstLinearFit
      thisAnalysis2 <- thisList$AnalysisResult$secondLinearFit
      coefficients <- summary(thisAnalysis1)$coefficients
      returnRow$InitialIntercept = coefficients[1,1]
      returnRow$InitialInterceptStdev = coefficients[1,2]
      returnRow$InitialSlope = coefficients[2,1]
      returnRow$InitialSlopeStdev = coefficients[2,2]
      coefficients2 <- summary(thisAnalysis2)$coefficients
      returnRow$SecondaryIntercept = coefficients2[1,1]
      returnRow$SecondaryInterceptStDev = coefficients2[1,2]
      returnRow$SecondarySlope = coefficients2[2,1]
      returnRow$SecondarySlopeStdev = coefficients2[2,2]
      returnRow$ElapsedTimeCutoff = thisList$AnalysisResult$SegmentationCutoff[1,2]
      returnRow$ElapsedTimeCutoffStdev = thisList$AnalysisResult$SegmentationCutoff[1,3]
      returnRow$stErrorOfEstimates = mean(thisAnalysis1$s, thisAnalysis2$s)
    }
    returnData <- rbind(returnData,returnRow)
    rm(thisList)
  }
  return(returnData)
}





#` This function takes the results from FitLumifugeFrontTrackingResults() and extracts
#` the fitted values which are then added to the dataset
ExtractFittedValuesFrontTrackFit <- function(theDataSet,ResultsList)
{
    FittedValuesDataFrame <- data.frame(UniqueSampleID = character(0), ElapsedTimeSec = numeric(0),Transmission = numeric(0), FittedDistance = numeric(0))
    for(i in 1:length(ResultsList))
    {
        ThisElapsedTimeSec <- NULL
        ThisFittedDistance <- NULL
        
        if(ResultsList[[i]]$analysisDetail == "rlm segmented")
        {
            ThisElapsedTimeSec <- c(ResultsList[[i]]$AnalysisResult$firstLinearFit$x[,2],ResultsList[[i]]$AnalysisResult$secondLinearFit$x[,2])
            ThisFittedDistance <- c(ResultsList[[i]]$AnalysisResult$firstLinearFit$fitted.values,ResultsList[[i]]$AnalysisResult$secondLinearFit$fitted.values)
        } else 
        {
            ThisElapsedTimeSec <- c(ResultsList[[i]]$AnalysisResult$x[,2])
            ThisFittedDistance <- c(ResultsList[[i]]$AnalysisResult$fitted.values)
        }
        
        FittedValuesDataFrame <- rbind(FittedValuesDataFrame,data.frame( UniqueSampleID = rep(ResultsList[[i]]$UniqueSampleID,
                                                                                              length(ThisFittedDistance)),
                                                                         Transmission = rep(ResultsList[[i]]$Transmission,
                                                                                            length(ThisFittedDistance)),
                                                                         ElapsedTimeSec = ThisElapsedTimeSec,
                                                                         FittedDistances = ThisFittedDistance))
    }
    theDataSet <- merge(x = theDataSet,y=FittedValuesDataFrame, by=c("UniqueSampleID","Transmission","ElapsedTimeSec"),all = TRUE)
    return(theDataSet)
}






## ------- Advanced Analysis - Integration -------------


#' This function fits the integration data as obtained from the Lumifuge
#' to an exponential decay function to obtain a kinetic parameter
#' since this non-linear fitting procedure is quite dependent on the initial
#' guesses of the fitting parameters I will use nl2 which tries different 
#' combinations of starting values to find the one with the best fit
FitIntegrationData <- function(IntegrationDataset)
{
  set.seed(123)
  library(nls2)
  theDecay <- Integral ~ A0 +  exp(-k*(TimeSec)) #A *
  StartingValues <- expand.grid(A0 = seq(0, 20, len = 2), 
                     k = c(0.0001,0.001,0.01))# A = seq(0, 10, len = 4), 
  
  mod1 <- nls2(theDecay,  start = StartingValues, algorithm = "brute-force",data = IntegrationDataset)
  
}











