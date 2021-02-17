# here we select which indexes must be considered, for the full list run this in the console: names(df)
startIndex <- 4
stopIndex <- 28
blackListIndexes <- c(5, 18)

# here we select which data frame is containing all the data
setwd('/Users/gianlucascoccia/Desktop/svn2/gianluca/MappingStaticAnalysisApps/replicationPackage/data/scripts')
data <- read.csv2("../data/rawData/data.csv", header = TRUE, quote = "\"", dec = ".", fill = TRUE, comment.char = "", na.strings = "AAAAAAAA")
outputFile <- "../output/horizontal.pdf"
dataSource <- data

# separator used in categorical variables with multiple values
separator <- ', '

############# DO NOT CHANGE ANYTHING BELOW THIS LINE ############# 

df <- dataSource

index <- 1
generateCouples <- function() {
  indexes <- c(startIndex:stopIndex)
  var1 <- c()
  var2 <- c()
  for(i in indexes) {
      if(!(i %in% blackListIndexes) && (i <= length(names(df)))) {
        var1el <- names(df)[i]
        indexes2 <- c(i:stopIndex)
        for(j in indexes2) {
          if(!(j %in% blackListIndexes) && (j <= length(names(df)))) {
            var2el <- names(df)[j]
            if(var1el != var2el) {
              var1 <- append(var1, var1el)
              var2 <- append(var2, var2el)
            }
          }
        }
      }
  }
  result <- data.frame(var1, var2)
  return(result)
} 

result <- generateCouples()

topScale <- nrow(dataSource)

pdf(outputFile, width=10, height=10)
par(mar=c(2, 2, 2, 2))
par(mfrow=c(1, 1))
par(las=1)

createPlot <- function(plotName, var1, var2) {

  library(splitstackshape)
  library(plyr)
  dt2 <- cSplit(df, splitCols=names(df), sep=separator, direction="long", drop=TRUE)
  dt2 <- dt2[!duplicated(dt2), ]

  ind <- apply(dt2, 1, function(x) all(is.na(x)))
  dt2 <- dt2[ !ind, ]
  
  index = 1
  for(i in 1:nrow(dt2)) {
    row <- dt2[i,]
    if(is.na(row$UID)) {
      dt2[[i,1]] <- dt2[[i-1,1]]
    } else {
      index <- i
    }
  }

  tbl <- table(as.factor(dt2[[as.character(var1)]]), as.factor(dt2[[as.character(var2)]]))
  resultDf <- as.data.frame.matrix(tbl)
  
  for(x in 1:nrow(tbl)) {
    xName <- rownames(tbl)[x]
    for(y in 1:ncol(tbl)) {
      yName <- colnames(tbl)[y]
      tbl[x,y] <- 0
      for(i in 1:nrow(data)) {
        row <- data[i,]
        if(grepl(xName, row[[as.character(var1)]]) && grepl(yName, row[[as.character(var2)]])) {
          tbl[x,y] <- tbl[x,y] + 1
        }
      }
    }
  }

  library(ggplot2)
  library(reshape2)

  plot <- ggplot(melt(as.factor(tbl)), aes(as.factor(Var2), as.factor(Var1))) +
    geom_tile(data=melt(tbl), aes(fill=as.integer(value)), color="grey") +
    geom_text(data=melt(tbl), aes(label=value), size=4, color="grey") +
    theme_bw() + 
    scale_fill_gradient2(low="blue", high="red", mid="white",name="Frequency", limit=c(0,topScale)) +
    theme(axis.text.x = element_text(angle=45, vjust=1, size=11, hjust=1)) +
    coord_equal() + labs(x=var2, y=var1) +
    ggtitle(plotName)

  print(plot)
}

#library(ggplot2)

resultLength <- nrow(result)
#for(i in 218:218) {
for(i in 1:resultLength) {
  plotName <- paste(result[i,]$var1, "_____", result[i,]$var2, sep="")
  print(paste(i, "/", resultLength, " - ", plotName))
  createPlot(plotName, result[i,]$var1, result[i,]$var2)
}

dev.off()
