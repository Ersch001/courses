

library(shiny)

require(datasets);data("mtcars");require(ggplot2);require(ggrepel)
prCData <- princomp(mtcars, cor = TRUE)
mtcars$PrComp1 <- prCData$scores[,1]
mtcars$PrComp2 <- prCData$scores[,2]


shinyServer(function(input, output) {

  output$mtCarsPlot <- renderPlot({
    thisCorSet <- subset(mtcars,hp >= input$Horses[1] & hp <= input$Horses[2] & am %in% input$transmission)
    if(length(thisCorSet[,1])>2)
    {
      carsHC <- hclust(dist(thisCorSet), method = "ward.D2")
      myk <- ifelse(input$cluster>=length(carsHC)-2,length(carsHC)-2,input$cluster)
      thisCorSet$cluster <- factor(cutree(carsHC, k = myk))
    }
    else
      thisCorSet$cluster <- factor(rep(1,length(thisCorSet[,1])))
    
    
    p1 <- ggplot(thisCorSet,aes(x=PrComp1, y=PrComp2)) +
      theme_classic() +
      geom_point(aes(color = cluster), alpha = 0.55, size = 3) +
      xlab("PC1") +
      ylab("PC2") + xlim(-4,4)+ ylim(-4,4)+ geom_text_repel(aes(y = PrComp2 + 0.25, label = rownames(thisCorSet),color = cluster)) 
    p1
  })

})
