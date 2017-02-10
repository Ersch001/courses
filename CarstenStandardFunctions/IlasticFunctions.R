#` This function reads the H5 file as exported from ilastic and returns the size of the oiling off area and the 
#` cheese area in a list
#` @H5File The exported H5 file obtained from Ilastik
#` @pixelArea The area of one pixel in the desired unit, e.g. mm^2/pixel to convert the area of the detected cheese and oiling off area into the desired unit 
getOilingOffSingleFile <- function(H5File,pixelArea = 1)
{
    library(rhdf5)
    rawData <- h5read(H5File,"exported_data")
    CheeseArea <- rawData[1,,]
    OilArea <- rawData[2,,]
    FilterPaper <- rawData[3,,]
    #Background <- rawData[4,,]
    
    ImageNumber <-as.numeric(gsub("IMG_","",regmatches(H5File, regexpr("IMG_....", H5File))))
    
    library(EBImage)
    
    thisImage <- readImage(gsub("_Probabilities______.h5",".jpg",H5File))
    thisImage <- channel(thisImage,"gray")
    grayValues <- thisImage * OilArea
    
    OilArea1 <- (sum(OilArea)+sum(CheeseArea))*pixelArea
    OilRing1 <- sum(OilArea) *pixelArea
    CheeseArea1 <- sum(CheeseArea)*pixelArea
    TotalArea1 <- (sum(OilArea)+sum(CheeseArea) + sum(FilterPaper))*pixelArea
    thislist <- list(OilArea = OilArea1,CheeseArea = CheeseArea1, TotalArea = TotalArea1,OilRing = OilRing1,
           MeanRelativeBrowning = mean(grayValues[grayValues >0]),MedianrelativeBrowning = median(grayValues[grayValues >0]),ImageNumber = ImageNumber)
    rm(thisImage)
    rm(grayValues)
    rm(rawData)
    return(thislist)
    
}


#` This function reads the H5 file as exported from ilastic and returns the results from
#` the analysis of the pizza images
#` @H5File The exported H5 file obtained from Ilastik
#` @pixelArea The area of one pixel in the desired unit, e.g. mm^2/pixel to convert the area into the desired unit 
getPizzaResultsSingleFile <- function(H5File,pixelArea = 1)
{
    library(rhdf5)
    rawData <- h5read(H5File,"exported_data")
    BlisterArea <- rawData[1,,] >0.3
    CheeseArea <- rawData[2,,]>0.3
    PizzaArea <- (1-rawData[3,,])>0.9
    TomatoSauce <- rawData[4,,]>0.3
    FreeFat <- rawData[5,,]>0.3
    # library(lattice)
    # image(rawData[1,,])
    
    
    ImageNumber <-as.numeric(gsub("IMG_","",regmatches(H5File, regexpr("IMG_....", H5File))))
    
    library(EBImage)
    
    thisImage <- readImage(gsub("_Probabilities_.h5",".jpg",H5File))
    thisImage <- channel(thisImage,"gray")*255
    
    TotalArea1 <- sum(PizzaArea)*pixelArea
    
    FreeFat1 <- sum(FreeFat)*pixelArea
    TomatoSauce1 <- sum(TomatoSauce) *pixelArea
    CheeseArea1 <- (sum(CheeseArea)+sum(FreeFat))*pixelArea
    BlisterArea1 <- sum(BlisterArea)*pixelArea
    BlisterIntensity <- BlisterArea*thisImage
    thislist <- list(CheeseArea = CheeseArea1,TomatoSauceArea = TomatoSauce1, BlisterArea = BlisterArea1, FreeFatArea = FreeFat1,TotalArea = TotalArea1,
                     meanBlisterIntensity = mean(BlisterIntensity[BlisterIntensity>0]),modeBlisterIntensity = median(BlisterIntensity[BlisterIntensity>0]), ImageNumber = ImageNumber)
    rm(thisImage)
    rm(BlisterIntensity)
    rm(rawData)
    return(thislist)
}

getMultiple <- function(Folder,AnalysisType , pixelArea = 1)
{
    analysisTypes = c("oilingOff","pizza")
    if(AnalysisType %in% analysisTypes)
    {
        RawFiles <- list.files(Folder,pattern = "*.h5", full.names = TRUE)
        returnData <- data.frame()
        for(i in 1:length(RawFiles))
        {
            if(AnalysisType %in% analysisTypes[1])
                thisData <- getOilingOffSingleFile(RawFiles[i],pixelArea)
            else if(AnalysisType %in% analysisTypes[2])
                thisData <- getPizzaResultsSingleFile(RawFiles[i],pixelArea)
            returnData <- merge(returnData,as.data.frame(thisData),all = TRUE)
        }
        return(returnData)
    } else
    {
        stop(paste("please choose one of the valid analysis types:",analysisTypes))
    }
}




