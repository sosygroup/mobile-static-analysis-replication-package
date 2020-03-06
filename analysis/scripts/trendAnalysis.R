setwd("/Users/gianlucascoccia/Desktop/svn/gianluca/MappingStaticAnalysisApps/data/scripts")
data <- read.csv("../rawData/data.csv", header = TRUE, quote = "\"", dec = ".", fill = TRUE, comment.char = "", na.strings = "-")

figuresPath <- "../../paper/figures/"

columnName <- "Macro.analysis.goal"
filePath <- paste(figuresPath, columnName, ".pdf", sep="")

tagsToConsider <- levels(factor(data[[columnName]]))

# se si vuole considerare solo un sottoinsieme dei tag (e.g., solo workshop papers e conference papers), 
# specificarli qui sotto e decommentare la riga. Tutto ci?? che non sta qui va nella categoria "Other"

#tagsToConsider <- c("E", "M", "I")

# qui ci vanno le coppie valore-label nel caso in cui vogliamo rinominare alcune label del parametro
prettyPrintedTags <- c("D"="Developer", 
                       "U"="User"
)

yearColumnName <- "Year.of.publication"
idColumnName <- "UID"

othersSupported <- FALSE

figureWidth <- 6 
figureHeight <- 3

maxYAxis <- nrow(data) / 2

# DO NOT CHANGE ANYTHING BELOW THIS LINE

separator <- ", "

tagsToConsider <- unlist(unique(as.list(strsplit(paste(tagsToConsider, sep=separator, collapse=separator), separator)[[1]])))

stageData <- function(tbName,tags) {
  apply <- ""
  cleanName <-gsub("\\.", " ", tbName) 
  caption <<- paste(apply,cleanName, sep=" ")
  column <<- tbName
  columnName <<- cleanName
  tags <<- tags
  cleanTags <<- tags
  for(i in 1:length(tags)) {
    if(tags[i] %in% names(prettyPrintedTags)) {
      cleanTags[i] <- prettyPrintedTags[[tags[i]]]
    }
  }
  cleanTags <<- gsub("\\_", " ", cleanTags)
  tagNames <<- cleanTags
}

stageData(columnName,tagsToConsider)

getPrimaryList <- function(currentTag) {
  assigned <- subset(data, grepl(currentTag, data[[column]]))[[idColumnName]]
  unassigned <<- setdiff(unassigned, assigned)
  return(assigned)
}

getPrimaryOccurrences <- function(currentTag, tagName) {
  tag <- tagName
  year <- subset(data, grepl(currentTag, data[[column]]))[[yearColumnName]]
  df <- data.frame(tag, year)
  return(df)
}

getRowOther <- function() {
  tag <- "Other"
  year <- data[[yearColumnName]][data[[idColumnName]] %in% unassigned]
  df <- data.frame(tag, year)
  return(df)
}

getRowTag <- function(currentTag, currentName) {
  currentList <- getPrimaryList(currentTag)
  return(c(currentName, currentList, currentTag))
}

occurrences <- data.frame(tag=character(), 
                          year=character(),
                          stringsAsFactors=TRUE)
unassigned <- data[[idColumnName]]

for(i in 1:length(tags)) {
  occurrences <- rbind(occurrences, getPrimaryOccurrences(tags[i], cleanTags[i]))
}

combined <- data.frame(tags, tagNames)
#combined <- combined[order(combined$occurrences, decreasing=T),]

for(i in 1:length(combined$tags)) {
  getRowTag(combined$tags[i], combined$tagNames[i])
}

if(othersSupported && length(unassigned != 0)) {
  occurrences <- rbind(toPlot, getRowOther())
}

library(ggplot2)
library(plyr)

toPlot <- as.data.frame(table(occurrences))
toPlot$year <- as.numeric(levels(toPlot$year))

plot <- ggplot(toPlot, aes(x=year, y=Freq, fill=tag)) + geom_area(colour="black", size=.2, alpha=.8) +
  scale_fill_brewer(palette="Greys", breaks=rev(levels(as.factor(toPlot$tag)))) + 
  scale_y_continuous(limits = c(0, maxYAxis)) +
  scale_x_continuous(breaks = round(seq(min(toPlot$year), max(toPlot$year), by = 1),1)) + 
  theme_classic() + theme(legend.title=element_blank()) + theme(legend.position="bottom") +
  theme(axis.title.x = element_blank()) + theme(axis.title.y = element_blank())
  #theme(panel.grid.major = element_line(colour = "lightgrey")) 

print(plot)
ggsave(filePath, plot, width=figureWidth, height=figureHeight, units="in", scale=1)

