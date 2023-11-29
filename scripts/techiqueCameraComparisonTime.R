library(plyr)
library(boot)
library(ggplot2)
library(stringr)

data_withouttraining <- subset(allTrials, training=="false")
data_withouttraining$technique <- as.factor(data_withouttraining$technique)    
data_withouttraining$cameraTransition <- as.factor(data_withouttraining$cameraTransition)
data_withouttraining$renderingOrder <- as.factor(data_withouttraining$renderingOrder)    
data_withouttraining$technique <- str_replace(data_withouttraining$technique, "Goto", "Animated")
data_withouttraining$technique <- str_replace(data_withouttraining$technique, "Draggo", "Dragalong")
data_withouttraining$renderingOrder <- str_replace(data_withouttraining$renderingOrder, "RF", "FF")

data_time<-subset(data_withouttraining, timeout=="false")


aggregated_time <- ddply(data_time,
                         c("participant","technique", "cameraTransition", "renderingOrder"),
                         summarise,
                         mean_value = mean(timeFromStart)
                         
)

aggregated_data <-aggregated_time
fileName <-"./routputs/comparisons/time/techniqueCameraComparisonsTime"

# Create three arrays
array1 <- c("Dragalong", "Animated")
array2 <- c("RF", "ZF")

# Create a data frame with all combinations
combinations_df <- expand.grid(array1, array2)
combresult_df <- data.frame()
participant <-c(0:23)
result_df <- data.frame()
for(i in 1:nrow(combinations_df)) {       # for-loop over rows
  print(combinations_df[i,])
  combination<-combinations_df[i,]
  elements <-aggregated_data
  elements<-subset(elements, technique==combination$Var1)
  elements<-subset(elements, cameraTransition==combination$Var2)
  elements$factors <- paste(elements$technique, elements$cameraTransition, sep = "_")
  participants_df<-data.frame(participant)
  participants_df$factors<-paste(combination$Var1, combination$Var2, sep = "_")
  statstable_data <- ddply(elements,
                           c("participant","factors"),
                           summarise,
                           value=mean(mean_value)
  )
  stastable_data_merged <- merge(participants_df, statstable_data, by = "participant", all.x = TRUE)
  stastable_data_merged <- subset(stastable_data_merged, select = -factors.y)
  print(stastable_data_merged)
  
  #stastable_time_merged[is.na(stastable_time_merged)] <- -1
  elements <- reshape(stastable_data_merged, timevar="factors.x", idvar=c("participant"), direction="wide")
  colnames(elements) <- gsub("value.", "", colnames(elements))
  result_column <- elements[, 2, drop=FALSE]
  print(result_column)
  #data2[i, ] <- data2[i, ] - 100
  if(ncol(result_df) == 0){
    result_df <- elements[, 1, drop=FALSE]
    result_df <- cbind(result_df, result_column)
    #rownames(readCounts) = d$Name
  } else{
    result_df <- cbind(result_df, result_column)
  }
}


columns = c("name", "pointEstimate", "ci_max", "ci_min", "level", "ci_corr_max", "ci_corr_min") 
comparisons = data.frame(matrix(nrow = 0, ncol = length(columns))) 
colnames(comparisons) = columns

for(i in 2:ncol(result_df)) {# for-loop over columns
  k<-i+1
  print("i")
  print(i)
  if (k<=ncol(result_df)) {
    for (j in k:ncol(result_df)) {
      print(j)
      avalues <- result_df[,i]
      bvalues <- result_df[,j]
      values <- data.frame(avalues,bvalues)
      values<-na.omit(values)
      #techniqueA <- bootstrapMeanCI(result_df[,i])
      #techniqueB <- bootstrapMeanCI(result_df[,j])
      #diffBA = bootstrapMeanCI_corr(result_df[,i] - result_df[,j], 12)
      diffBA = bootstrapMeanCI_corr(values$avalues - values$bvalues, 12)
      analysisData <- c()
      analysisData$name <- c(paste(colnames(result_df)[i], colnames(result_df)[j],sep="-")) 
      analysisData$pointEstimate <- c(diffBA[1])
      analysisData$ci.max <- c(diffBA[2])
      analysisData$ci.min <- c(diffBA[3])
      analysisData$level <- c(diffBA[4])
      analysisData$ci_corr.max <- c(diffBA[5])
      analysisData$ci_corr.min <- c(diffBA[6])
      
      datatoprint <- data.frame(factor(analysisData$name), analysisData$pointEstimate, analysisData$ci.max, analysisData$ci.min, analysisData$level, analysisData$ci_corr.max, analysisData$ci_corr.min)
      colnames(datatoprint) <- c("technique", "mean_time", "lowerBound_CI", "upperBound_CI", "corrected_CI", "lowerBound_CI_corr", "upperBound_CI_corr") #We use the name mean_time for the value of the mean even though it's not a time, it's just to parse the data for the plot
      comparisons = rbind(comparisons,c(name=analysisData$name, pointEstimate=analysisData$pointEstimate, ci_max=analysisData$ci.max, 
                                        ci_min=analysisData$ci.min, level= analysisData$level, ci_corr_max= analysisData$ci_corr.max, ci_corr_min=analysisData$ci_corr.min))
      barChart(datatoprint, analysisData$name, nbTechs = 1, ymin = -10, ymax = 10, mycolor = "steelblue3", "", "")
      ggsave(paste(fileName, analysisData$name, ".pdf", sep="_"),width = 20, height = 5, units = "cm")
    }
  }
}

colnames(comparisons) = columns
write.table(comparisons, paste(fileName, ".txt", sep="_"), sep=",",row.names=FALSE)
