library(plyr)
library(boot)
library(ggplot2)

data_withouttraining <- subset(allTrials, training=="false")
data_withouttraining$technique <- as.factor(data_withouttraining$technique)    
data_withouttraining$cameraTransition <- as.factor(data_withouttraining$cameraTransition)
data_withouttraining$renderingOrder <- as.factor(data_withouttraining$renderingOrder)   
data_withouttraining$technique <- str_replace(data_withouttraining$technique, "Goto", "Animated")
data_withouttraining$technique <- str_replace(data_withouttraining$technique, "Draggo", "Dragalong")
data_withouttraining$renderingOrder <- str_replace(data_withouttraining$renderingOrder, "RF", "FF")

aggregated_error <- ddply(data_withouttraining,
                          c("participant","technique", "cameraTransition", "renderingOrder"),
                          summarise,
                          mean_error_measure = mean(firstErrorMeasure)
                          
)

#Technique
elements <- aggregated_error
elements <- elements [ order(elements$participant, elements$technique), ]
statstable_error <- ddply(elements,
                          c("participant","technique"),
                          summarise,
                          errorMeasure=mean(mean_error_measure)
)

elements <- statstable_error
elements <- reshape(elements, timevar="technique", idvar=c("participant"), direction="wide")
colnames(elements) <- gsub("errorMeasure.", "", colnames(elements))
elements <- na.omit(elements)

data <- elements
techniqueA <- bootstrapMeanCI(data$Dragalong)
techniqueB <- bootstrapMeanCI(data$Animated)

analysisData <- c()

analysisData$name <- c("Drag-along","Animated")
analysisData$pointEstimate <- c(techniqueA[1], techniqueB[1])
analysisData$ci.max <- c(techniqueA[3], techniqueB[3])
analysisData$ci.min <- c(techniqueA[2], techniqueB[2])

datatoprint <- data.frame(factor(analysisData$name),analysisData$pointEstimate, analysisData$ci.min, analysisData$ci.max)
colnames(datatoprint) <- c("Technique", "mean_time", "lowerBound_CI", "upperBound_CI ") #We use the name mean_time for the value of the mean even though it's not a time, it's just to parse the data for the plot

write.table(datatoprint,"./routputs/tehnique_errormeasure.txt", sep=",",row.names=FALSE)

barChart(datatoprint, analysisData$name, nbTechs = 2, ymin = 0, ymax = 0.25, mycolor = "steelblue3", "", "")
ggsave("./routputs/technique_errormeasure.pdf", width = 20, height = 10, units = "cm")

diffBA = bootstrapMeanCI_corr(data$Dragalong - data$Animated, 2)

analysisData <- c()
analysisData$name <- c("Drag-along - Animated") 
analysisData$pointEstimate <- c(diffBA[1])
analysisData$ci.max <- c(diffBA[2])
analysisData$ci.min <- c(diffBA[3])
analysisData$level <- c(diffBA[4])
analysisData$ci_corr.max <- c(diffBA[5])
analysisData$ci_corr.min <- c(diffBA[6])

datatoprint <- data.frame(factor(analysisData$name), analysisData$pointEstimate, analysisData$ci.max, analysisData$ci.min, analysisData$level, analysisData$ci_corr.max, analysisData$ci_corr.min)
colnames(datatoprint) <- c("technique", "mean_time", "lowerBound_CI", "upperBound_CI", "corrected_CI", "lowerBound_CI_corr", "upperBound_CI_corr") #We use the name mean_time for the value of the mean even though it's not a time, it's just to parse the data for the plot
write.table(datatoprint,"./routputs/technique_errormeasure_diff.txt", sep=",",row.names=FALSE)
barChart(datatoprint, analysisData$name, nbTechs = 1, ymin = -0.5, ymax = 0.5, mycolor = "steelblue3", "", "")
ggsave("./routputs/technique_errormeasure_diff.pdf", width = 20, height = 5, units = "cm")

### Camera animation

elements <- aggregated_error
elements <- elements [ order(elements$participant, elements$cameraTransition), ]
statstable_error <- ddply(elements,
                          c("participant","cameraTransition"),
                          summarise,
                          errorMeasure=mean(mean_error_measure)
)
elements <- statstable_error
elements <- reshape(elements, timevar="cameraTransition", idvar=c("participant"), direction="wide")
colnames(elements) <- gsub("errorMeasure.", "", colnames(elements))
elements <- na.omit(elements)

data <- elements
techniqueA <- bootstrapMeanCI(data$RF)
techniqueB <- bootstrapMeanCI(data$ZF)

analysisData <- c()

analysisData$name <- c("RF","ZF")
analysisData$pointEstimate <- c(techniqueA[1], techniqueB[1])
analysisData$ci.max <- c(techniqueA[3], techniqueB[3])
analysisData$ci.min <- c(techniqueA[2], techniqueB[2])

datatoprint <- data.frame(factor(analysisData$name),analysisData$pointEstimate, analysisData$ci.min, analysisData$ci.max)
colnames(datatoprint) <- c("Camera Animation", "mean_time", "lowerBound_CI", "upperBound_CI ") #We use the name mean_time for the value of the mean even though it's not a time, it's just to parse the data for the plot


write.table(datatoprint, "./routputs/camera_errormeasure.txt", sep=",",row.names=FALSE)

barChart(datatoprint, analysisData$name, nbTechs = 2, ymin = 0, ymax = 0.25, mycolor = "steelblue3", "", "")
ggsave("./routputs/camera_errormeasure.pdf", width = 20, height = 10, units = "cm")

diffBA = bootstrapMeanCI_corr(data$RF - data$ZF, 2)

analysisData <- c()
analysisData$name <- c("RF-ZF") 
analysisData$pointEstimate <- c(diffBA[1])
analysisData$ci.max <- c(diffBA[3])
analysisData$ci.min <- c(diffBA[2])
analysisData$level <- c(diffBA[4])
analysisData$ci_corr.max <- c(diffBA[6])
analysisData$ci_corr.min <- c(diffBA[5])

datatoprint <- data.frame(factor(analysisData$name), analysisData$pointEstimate, analysisData$ci.max, analysisData$ci.min, analysisData$level, analysisData$ci_corr.max, analysisData$ci_corr.min)
colnames(datatoprint) <- c("Camera Animation", "mean_time", "lowerBound_CI", "upperBound_CI", "corrected_CI", "lowerBound_CI_corr", "upperBound_CI_corr") #We use the name mean_time for the value of the mean even though it's not a time, it's just to parse the data for the plot
write.table(datatoprint, "./routputs/camera_errormeasure_diff.txt", sep=",",row.names=FALSE)
barChart(datatoprint, analysisData$name, nbTechs = 1, ymin = -0.5, ymax = 0.5, mycolor = "steelblue3", "", "")
ggsave("./routputs/camera_errormeasure_diff.pdf", width = 20, height = 5, units = "cm")

### Rendering Order

elements <- aggregated_error
elements <- elements [ order(elements$participant, elements$renderingOrder), ]
statstable_error <- ddply(elements,
                          c("participant","renderingOrder"),
                          summarise,
                          errorMeasure=mean(mean_error_measure)
)
elements <- statstable_error
elements <- reshape(elements, timevar="renderingOrder", idvar=c("participant"), direction="wide")
colnames(elements) <- gsub("errorMeasure.", "", colnames(elements))
elements <- na.omit(elements)

data <- elements
techniqueA <- bootstrapMeanCI(data$CF)
techniqueB <- bootstrapMeanCI(data$Coupled)
techniqueC <- bootstrapMeanCI(data$FF)

analysisData <- c()

analysisData$name <- c("CF","Coupled", "FF")
analysisData$pointEstimate <- c(techniqueA[1], techniqueB[1], techniqueC[1])
analysisData$ci.max <- c(techniqueA[3], techniqueB[3], techniqueC[3])
analysisData$ci.min <- c(techniqueA[2], techniqueB[2], techniqueC[2])

datatoprint <- data.frame(factor(analysisData$name),analysisData$pointEstimate, analysisData$ci.min, analysisData$ci.max)
colnames(datatoprint) <- c("Rendering Order", "mean_time", "lowerBound_CI", "upperBound_CI ") #We use the name mean_time for the value of the mean even though it's not a time, it's just to parse the data for the plot


write.table(datatoprint, "./routputs/order_errormeasure.txt", sep=",",row.names=FALSE)

barChart(datatoprint, analysisData$name, nbTechs = 3, ymin = 0, ymax = 0.25, mycolor = "steelblue3", "", "")
ggsave("./routputs/order_errormeasure.pdf", width = 20, height = 10, units = "cm")

diffAB = bootstrapMeanCI_corr(data$CF - data$Coupled, 3)
diffAC = bootstrapMeanCI_corr(data$CF - data$FF, 3)
diffBC = bootstrapMeanCI_corr(data$Coupled - data$FF, 3)

analysisData <- c()
analysisData$name <- c("CF-Coupled","CF-FF","Coupled-FF")
analysisData$pointEstimate <- c(diffAB[1], diffAC[1], diffBC[1])
analysisData$ci.max <- c(diffAB[3],diffAC[3], diffBC[3])
analysisData$ci.min <- c(diffAB[2],diffAC[2], diffBC[2])
analysisData$level <- c(diffAB[4],diffAC[4], diffBC[4])
analysisData$ci_corr.max <- c(diffAB[6],diffAC[6], diffBC[6])
analysisData$ci_corr.min <- c(diffAB[5],diffAC[5], diffBC[5])

datatoprint <- data.frame(factor(analysisData$name), analysisData$pointEstimate, analysisData$ci.max, analysisData$ci.min, analysisData$level, analysisData$ci_corr.max, analysisData$ci_corr.min)
colnames(datatoprint) <- c("Rendering Order", "mean_time", "lowerBound_CI", "upperBound_CI", "corrected_CI", "lowerBound_CI_corr", "upperBound_CI_corr") #We use the name mean_time for the value of the mean even though it's not a time, it's just to parse the data for the plot
write.table(datatoprint, "./routputs/order_errormeasure_diff.txt", sep=",",row.names=FALSE)
barChart(datatoprint, analysisData$name, nbTechs = 3, ymin = -0.25, ymax = 0.25, mycolor = "steelblue3", "", "")
ggsave("./routputs/order_errormeasure_diff.pdf", width = 20, height = 10, units = "cm")


#Technique x Transition order
plotRenderingOrderTechnique<- function(techniqueName) {
  elements <- aggregated_error
  elements <- subset(elements, technique==techniqueName)
  elements <- elements [ order(elements$participant, elements$renderingOrder), ]
  statstable_error <- ddply(elements,
                            c("participant","renderingOrder"),
                            summarise,
                            errorMeasure=mean(mean_error_measure)
  )
  elements <- statstable_error
  elements <- reshape(elements, timevar="renderingOrder", idvar=c("participant"), direction="wide")
  colnames(elements) <- gsub("errorMeasure.", "", colnames(elements))
  elements <- na.omit(elements)
  
  data <- elements
  techniqueA <- bootstrapMeanCI(data$CF)
  techniqueB <- bootstrapMeanCI(data$Coupled)
  techniqueC <- bootstrapMeanCI(data$FF)
  
  analysisData <- c()
  
  analysisData$name <- c("CF","Coupled", "FF")
  analysisData$pointEstimate <- c(techniqueA[1], techniqueB[1], techniqueC[1])
  analysisData$ci.max <- c(techniqueA[3], techniqueB[3], techniqueC[3])
  analysisData$ci.min <- c(techniqueA[2], techniqueB[2], techniqueC[2])
  
  datatoprint <- data.frame(factor(analysisData$name),analysisData$pointEstimate, analysisData$ci.min, analysisData$ci.max)
  colnames(datatoprint) <- c("Rendering Order", "mean_time", "lowerBound_CI", "upperBound_CI ") #We use the name mean_time for the value of the mean even though it's not a time, it's just to parse the data for the plot
  write.table(datatoprint, paste("./routputs/order",techniqueName,"errorMeasure.txt", sep="_"), sep=",",row.names=FALSE)
  barChart(datatoprint, analysisData$name, nbTechs = 3, ymin = 0, ymax = 0.25, mycolor = "steelblue3", "", "")
  ggsave(paste("./routputs/order",techniqueName,"errorMeasure.pdf", sep="_"), width = 20, height = 10, units = "cm")
 
}

plotRenderingOrderTechnique("Dragalong")
plotRenderingOrderTechnique("Animated")

#Technique x camera transition
plotCameraTransitionTechnique<- function(techniqueName) {
  elements <- aggregated_error
  elements <- subset(elements, technique==techniqueName)
  elements <- elements [ order(elements$participant, elements$cameraTransition), ]
  statstable_error <- ddply(elements,
                            c("participant","cameraTransition"),
                            summarise,
                            errorMeasure=mean(mean_error_measure)
  )
  elements <- statstable_error
  
  print(summary(statstable_error))
  #
  elements <- reshape(elements, timevar="cameraTransition", idvar=c("participant"), direction="wide")
  colnames(elements) <- gsub("errorMeasure.", "", colnames(elements))
  elements <- na.omit(elements)
  
  data <- elements
  techniqueA <- bootstrapMeanCI(data$RF)
  techniqueB <- bootstrapMeanCI(data$ZF)
  
  analysisData <- c()
  
  analysisData$name <- c("RF","ZF")
  analysisData$pointEstimate <- c(techniqueA[1], techniqueB[1])
  analysisData$ci.max <- c(techniqueA[3], techniqueB[3])
  analysisData$ci.min <- c(techniqueA[2], techniqueB[2])
  
  datatoprint <- data.frame(factor(analysisData$name),analysisData$pointEstimate, analysisData$ci.min, analysisData$ci.max)
  colnames(datatoprint) <- c("Camera Animation", "mean_time", "lowerBound_CI", "upperBound_CI ") #We use the name mean_time for the value of the mean even though it's not a time, it's just to parse the data for the plot
  write.table(datatoprint, paste("./routputs/camera",techniqueName,"errorMeasure.txt", sep="_"), sep=",",row.names=FALSE)
  barChart(datatoprint, analysisData$name, nbTechs = 2, ymin = 0, ymax = 0.25, mycolor = "steelblue3", "", "")
  ggsave(paste("./routputs/camera",techniqueName,"errorMeasure.pdf", sep="_"), width = 20, height = 10, units = "cm")
}

plotCameraTransitionTechnique("Dragalong")
plotCameraTransitionTechnique("Animated")

#Camera Animation x Transition Order
plotCameraTransitionRenderingOrder<- function(renderingOrderName) {
  elements <- aggregated_error
  elements <- subset(elements, renderingOrder==renderingOrderName)
  elements <- elements [ order(elements$participant, elements$cameraTransition), ]
  statstable_error <- ddply(elements,
                            c("participant","cameraTransition"),
                            summarise,
                            errorMeasure=mean(mean_error_measure)
  )
  elements <- statstable_error
  
  print(summary(statstable_error))
  #
  elements <- reshape(elements, timevar="cameraTransition", idvar=c("participant"), direction="wide")
  colnames(elements) <- gsub("errorMeasure.", "", colnames(elements))
  elements <- na.omit(elements)
  
  data <- elements
  techniqueA <- bootstrapMeanCI(data$RF)
  techniqueB <- bootstrapMeanCI(data$ZF)
  
  analysisData <- c()
  
  analysisData$name <- c("RF","ZF")
  analysisData$pointEstimate <- c(techniqueA[1], techniqueB[1])
  analysisData$ci.max <- c(techniqueA[3], techniqueB[3])
  analysisData$ci.min <- c(techniqueA[2], techniqueB[2])
  
  datatoprint <- data.frame(factor(analysisData$name),analysisData$pointEstimate, analysisData$ci.min, analysisData$ci.max)
  colnames(datatoprint) <- c("Camera Animation", "mean_time", "lowerBound_CI", "upperBound_CI ") #We use the name mean_time for the value of the mean even though it's not a time, it's just to parse the data for the plot
  write.table(datatoprint, paste("./routputs/camera",renderingOrderName,"errorMeasure.txt", sep="_"), sep=",",row.names=FALSE)
  barChart(datatoprint, analysisData$name, nbTechs = 2, ymin = 0, ymax = 0.25, mycolor = "steelblue3", "", "")
  ggsave(paste("./routputs/camera",renderingOrderName,"errorMeasure.pdf", sep="_"), width = 20, height = 10, units = "cm")
  
}
plotCameraTransitionRenderingOrder("CF")
plotCameraTransitionRenderingOrder("FF")
plotCameraTransitionRenderingOrder("Coupled")

# Technique x Transition Order x Camera animation

plotCameraTransitionOrderTechnique<- function(renderingOrderName, techniqueName) {
  elements <- aggregated_error
  elements <- subset(elements, technique==techniqueName)
  elements <- subset(elements, renderingOrder==renderingOrderName)
  elements <- elements [ order(elements$participant, elements$renderingOrder), ]
  statstable_error <- ddply(elements,
                            c("participant","cameraTransition"),
                            summarise,
                            errorMeasure=mean(mean_error_measure)
  )
  elements <- statstable_error
  
  print(summary(statstable_error))
  #
  elements <- reshape(elements, timevar="cameraTransition", idvar=c("participant"), direction="wide")
  colnames(elements) <- gsub("errorMeasure.", "", colnames(elements))
  elements <- na.omit(elements)
  
  data <- elements
  techniqueA <- bootstrapMeanCI(data$RF)
  techniqueB <- bootstrapMeanCI(data$ZF)
  
  analysisData <- c()
  
  analysisData$name <- c("RF","ZF")
  analysisData$pointEstimate <- c(techniqueA[1], techniqueB[1])
  analysisData$ci.max <- c(techniqueA[3], techniqueB[3])
  analysisData$ci.min <- c(techniqueA[2], techniqueB[2])
  
  datatoprint <- data.frame(factor(analysisData$name),analysisData$pointEstimate, analysisData$ci.min, analysisData$ci.max)
  colnames(datatoprint) <- c("Camera Animation", "mean_time", "lowerBound_CI", "upperBound_CI ") #We use the name mean_time for the value of the mean even though it's not a time, it's just to parse the data for the plot
  
  filenametable= paste("./routputs/",techniqueName, renderingOrderName, "camera_errorCount.txt", sep="_")
  write.table(datatoprint,filenametable, sep=",",row.names=FALSE)
  
  barChart(datatoprint, analysisData$name, nbTechs = 2, ymin = 0, ymax = 0.3, mycolor = "steelblue3", "", "")
  filename= paste("./routputs/",techniqueName, renderingOrderName, "camera_errorMeasure.pdf", sep="_")
  ggsave(filename, width = 20, height = 5, units = "cm")
  
}

plotCameraTransitionOrderTechnique("Coupled", "Animated")
plotCameraTransitionOrderTechnique("FF", "Animated")
plotCameraTransitionOrderTechnique("CF", "Animated")
plotCameraTransitionOrderTechnique("Coupled", "Dragalong")
plotCameraTransitionOrderTechnique("FF", "Dragalong")
plotCameraTransitionOrderTechnique("CF", "Dragalong")  

