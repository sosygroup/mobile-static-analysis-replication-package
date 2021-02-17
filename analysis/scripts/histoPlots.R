setwd('/Users/gianlucascoccia/Desktop/svn2/gianluca/MappingStaticAnalysisApps/replicationPackage/data/scripts')

library(forcats)
library(stringr)

data <- read.csv2("../data/rawData/data.csv", header = TRUE, quote = "\"", fill = TRUE, comment.char = "", na.strings = "n_i")

figuresPath <- "../../../JISA_2020/figures/"


createPlot <- function(columnName, tagsToConsider, prettyPrintedTags, othersSupported, leftMargin, fileName, otherString, width, height) {
  
  filePath <- paste(figuresPath, fileName, ".pdf", sep="")
  print(filePath)
  
  pdf(filePath, width=width, height=height)
  par(mar=c(3, leftMargin, 1, 1))
  par(mfrow=c(1, 1))
  par(las=1)
  
  idColumnName <- "UID"
  
  # DO NOT CHANGE ANYTHING BELOW THIS LINE
  
  separator <- ", "
  
  tagsToConsider <- unlist(unique(as.list(strsplit(paste(tagsToConsider, sep=separator, collapse=separator), separator)[[1]])))
  
  addOther <- function(x){
    if(is.factor(x)) return(factor(x, levels=c(levels(x), otherString)))
    return(x)
  }
  
  createTable <- function(tbName,tags) {
    apply <- ""
    cleanName <-gsub("\\.", " ", tbName) 
    caption <<- paste(apply,cleanName, sep=" ")
    column <<- tbName
    columnName <<- cleanName
    tags <<- tags
    cleanTags <<- tags
    for(i in 1:length(tags)) {
      if(tags[i] %in% names(prettyPrintedTags)) {
        #print(prettyPrintedTags[[tags[i]]])
        cleanTags[i] <- prettyPrintedTags[[tags[i]]]
      }
    }
    cleanTags <<- gsub("\\_", " ", tags)
    tagNames <<- cleanTags
  }
  
  createTable(columnName,tagsToConsider)
  
  getPrimaryOccurrences <- function(currentTag) {
    assigned <- subset(data, grepl(currentTag, data[[column]]))[[idColumnName]]
    
    unassigned <<- setdiff(unassigned, assigned)
    
    #print(currentTag)
    #print(length(subset(data, grepl(currentTag, data[[column]]))[[idColumnName]]))
    
    return(length(subset(data, grepl(currentTag, data[[column]]))[[idColumnName]]))
  }
  
  combinedAddOthersRow <- function(df1) {
    otherList <- unassigned
    df1 <- as.data.frame(lapply(df1, addOther))
    df1 <- rbind(df1, c(otherString, otherString, length(otherList)))
    return(df1)
  }
  
  rerolledAddOthersRow <- function(df2) {
    otherList <- unassigned
    df2 <- unlist(lapply(df2, addOther))
    df2 <- as.character(df2)
    df2 <- append(df2, rep(otherString, length(otherList)))
    df2 <- as.factor(df2)
    return(df2)
  }
  
  reroll <- function(df) {
    rerolled <- c()
    for(i in 1:nrow(df)) {
      for(j in 1:df[i,][["occurrences"]]) {
        rerolled <- append(rerolled, toString(df[i,][["tagNames"]]))
      }
    }
    result <- data.frame(rerolled)
  }
  
  occurrences <- c()
  unassigned <- data[[idColumnName]]
  
  for(i in 1:length(tags)) {
    occurrences[i] <- getPrimaryOccurrences(tags[i])
  }
  
  combined <- data.frame(tags, tagNames, occurrences)
  
  rerolled <- reroll(combined)
  
  if(othersSupported && length(unassigned != 0)) {
    combined <- combinedAddOthersRow(combined)
    rerolled <- rerolledAddOthersRow(rerolled)
  }
  
  combined <- combined[order(combined$occurrences, decreasing=F),]
  if(othersSupported && length(unassigned != 0)) {
    rerolled <- fct_rev(fct_infreq(rerolled))
  } else {
    rerolled <- fct_rev(fct_infreq(rerolled$rerolled))
  }
  
  labels = sort(as.numeric(combined$occurrences), decreasing = FALSE) 
  
  plot <- plot(rerolled, main="", cex.main=1, xlim=c(0, nrow(data) + 15), cex=2, cex.names=1.9, las=1, horiz=TRUE)
  text(x=labels + 12, y = plot, label = labels, cex = 2, col = "black")
  
  dev.off()
}

######## RQ1 -> Most Targeted Venues 
columnName <- "Publication.venue.name"
tagsToConsider <- c("ASE", "ICSE", "CCS", "NDSS", "SAC", "ISSTA", "SPSM")
prettyPrintedTags <- c("ASE" = "ASE", 
                       "ICSE" = "ICSE", 
                       "CCS" = "CCS", 
                       "NDSS" = "NDSS",
                       "SAC" = "ACM SAC",
                       "ISSTA" = "ISSTA", 
                       "SPSM" = "SPSM")
createPlot(columnName, tagsToConsider, prettyPrintedTags, TRUE, 9, "MostTargetedVenues", "Other", 10, 4)

######## RQ1 -> Publication venue type
columnName <- "Publication.venue.type"
tagsToConsider <- levels(as.factor(data[[columnName]]))
prettyPrintedTags <- tagsToConsider
createPlot(columnName, tagsToConsider, prettyPrintedTags, FALSE, 10, "PublicationVenueType", "Other", 10, 3)

######## RQ1 -> Analysis goal 
columnName <- "Analysis.goal"
tagsToConsider <- levels(as.factor(data[[columnName]]))
prettyPrintedTags <- c(
  'ANTIPATTERNS' = 'Antipatterns',
  'ENERGY' = 'Energy',
  'PERFORMANCE' = 'Performance',
  'RESOURCE' = 'Resource',
  'FRAMEWORK' = 'Framework',
  'INTER-COMPONENT COMMUNICATION' = 'Inter-component communication',
  'INTER-APP COMMUNICATION' = 'Inter-app communication',
  'PRIVACY' = 'Privacy',
  'MALWARE' = 'Malware',
  'TESTING' = 'Testing',
  'REFACTORING' = 'Refactoring',
  'REFLECTION' = 'Reflection',
  'AUTHORSHIP' = 'Authorship',
  'SIMILARITY' = 'Similarity',
  'OBFUSCATOR IDENTIFICATION' = 'Obfuscation',
  'LIBRARY IDENTIFICATION' = 'Library detection'
)
createPlot(columnName, tagsToConsider, prettyPrintedTags, FALSE, 25, "AnalysisGoal", "Other", 12, 9)

######## RQ1 -> Macro analysis goal  
columnName <- "Macro.analysis.goal"
tagsToConsider <- levels(as.factor(data[[columnName]]))
prettyPrintedTags <- c("E" = "External quality",
                       "I" = "Internal quality",
                       "M" = "Improvement of \nmethodology")
createPlot(columnName, tagsToConsider, prettyPrintedTags, FALSE, 13.5, "MacroAnalysisGoal", "Other", 10, 3)

######## RQ1 -> Paper goal 
columnName <- "Paper.goal"
tagsToConsider <- levels(as.factor(data[[columnName]]))
prettyPrintedTags <- c("Q" = "Quality attribute \nassessment",
                       "I" = "Improvement of \nmethodology")
createPlot(columnName, tagsToConsider, prettyPrintedTags, FALSE, 14, "PaperGoal", "Other", 10, 2.75)

######## RQ2 -> Platform specificity 
columnName <- "Platform.specific"
tagsToConsider <- levels(as.factor(data[[columnName]]))
prettyPrintedTags <- c("A" = "Android",
                       "I" = "iOS",
                       "G" = "Generic")
createPlot(columnName, tagsToConsider, prettyPrintedTags, FALSE, 7.5, "PlatformSpecific", "Other", 10, 3)

######## RQ2 -> Implementation 
columnName <- "Implementation"
tagsToConsider <- levels(as.factor(data[[columnName]]))
prettyPrintedTags <- c("A" = "Android",
                       "I" = "iOS",
                       "G" = "Generic",
                       "T" = "Other")
createPlot(columnName, tagsToConsider, prettyPrintedTags, TRUE, 7.5, "Implementation", "Other", 11, 3)

######## RQ2 -> Static Hybrid approach 
columnName <- "Static.Hybrid.approach"
tagsToConsider <- levels(as.factor(data[[columnName]]))
prettyPrintedTags <- c("S" = "Static",
                       "H" = "Hybrid")
createPlot(columnName, tagsToConsider, prettyPrintedTags, TRUE, 7, "StaticHybridApproach", "Other", 10, 3)

######## RQ2 -> Machine Learning 
columnName <- "Machine.Learning"
tagsToConsider <- c(0,1)
prettyPrintedTags <- c("0" = "No",
                       "1" = "Yes")
createPlot(columnName, tagsToConsider, prettyPrintedTags, TRUE, 4, "MachineLearning", "Other", 10, 3)

######## RQ2 -> App artifact 
columnName <- "App.artifact"
tagsToConsider <- c("APK","SOURCE CODE")
prettyPrintedTags <- c("APK" = "Binary",
                       "SOURCE CODE" = "Source\ncode")
createPlot(columnName, tagsToConsider, prettyPrintedTags, FALSE, 7, "AppArtifact", "Other", 10, 3)

######## RQ2 -> Additional input 
columnName <- "Additional.input"
tagsToConsider <- levels(as.factor(data[[columnName]]))[-1]
prettyPrintedTags <- c("Policies" = "Policies & rules")
createPlot(columnName, tagsToConsider, prettyPrintedTags, FALSE, 18, "AdditionalInput", "Other", 10, 10)

######## RQ2 -> Analysis pre steps
columnName <- "Analysis.pre.steps"
tagsToConsider <- c(0,1)
prettyPrintedTags <- c("0" = "No", "1" = "Yes")
createPlot(columnName, tagsToConsider, prettyPrintedTags, FALSE, 4, "AnalysisPreSteps", "Other", 10, 3)

######## RQ2 -> Analysis technique 
columnName <- "Analysis.type"
data$Analysis.type = str_replace(data$Analysis.type, "CLASS\\b", "CLASS ANALYSIS")
tagsToConsider <- c("ABSTRACT INTERPRETATION",                                           
                    "NULLNESS", 
                    "TERMINATION ANALYSIS",                               
                    "CODE INSTRUMENTATION",                        
                    "STATISTICAL ANALYSIS",                                
                    "SYMBOLIC EXECUTION",                                   
                    "PATTERN-BASED CONTEXTUAL ANALYSIS",                                 
                    "RESPONSIVENESS ANALYSIS",                                           
                    "STRING ANALYSIS", 
                    "CONSTANT PROPAGATION",                             
                    "TYPE INFERENCE", 
                    "POINTS-TO ANALYSIS",
                    "SYMBOLIC EXECUTION",      
                    "DATA MINING", 
                    "CLASSIFICATION",
                    "FLOW ANALYSIS",
                    "MODEL BASED", 
                    "TAINT ANALYSIS", 
                    "SLICING",
                    "CLASS ANALYSIS",
                    "SIMILARITY-BASED",
                    "FORMAL ANALYSIS",
                    "OPCODE ANALYSIS",
                    "MODEL CHECKING",                               
                    "TYPESTATE ANALYSIS")
prettyPrintedTags <- c("FLOW ANALYSIS" = "Flow analysis",
                       "TAINT ANALYSIS" = "Taint analysis",
                       "DATA MINING" = "Data mining",
                       "CLASS ANALYSIS" = "Class analysis",
                       "CLASSIFICATION" = "Classification",
                       "SLICING" = "Slicing",
                       "MODEL BASED" = "Model-based analysis",
                       "STRING ANALYSIS" = "String analysis",
                       "ABSTRACT INTERPRETATION" = "Abstract interpretation",
                       "CONSTANT PROPAGATION" = "Constant propagation",
                       "TYPE INFERENCE" = "Type inference",
                       "CODE INSTRUMENTATION" = "Code instrumentation",
                       "SYMBOLIC EXECUTION" = "Symbolic execution",
                       "POINTS-TO ANALYSIS" = "Points-to analysis",
                       "NULLNESS" = "Nullness analysis",
                       "PATTERN-BASED CONTEXTUAL ANALYSIS" = "Pattern-based analysis",
                       "RESPONSIVENESS ANALYSIS" = "Responsiveness analysis",
                       "STATISTICAL ANALYSIS" = "Statistical analysis",
                       "TERMINATION ANALYSIS" = "Termination analysis",
                       "TYPESTATE ANALYSIS" = "Typestate analysis",
                       "SIMILARITY-BASED" = "Similarity-based analysis",
                       "FORMAL ANALYSIS" = "Formal analysis",
                       "OPCODE ANALYSIS" = "Opcode analysis",
                       "MODEL CHECKING" = "Model checking")
createPlot(columnName, tagsToConsider, prettyPrintedTags, FALSE, 20, "AnalysisTechnique", "Other", 10, 10)

######## RQ3 -> Target stakeholder 
columnName <- "Stakeholder"
tagsToConsider <- levels(as.factor(data[[columnName]]))
prettyPrintedTags <- c("D" = "App\ndeveloper", "A" = "Platform\nvendor", "R" = "Researcher", "U" = "User")
createPlot(columnName, tagsToConsider, prettyPrintedTags, FALSE, 10, "TargetStakeholder", "Other", 10, 5)

######## RQ3 -> Tool availability
columnName <- "Tool.availability"
tagsToConsider <- c(0,1)
prettyPrintedTags <- c("0" = "No", "1" = "Yes")
createPlot(columnName, tagsToConsider, prettyPrintedTags, FALSE, 5, "ToolAvailability", "Other", 10, 3)

######## RQ3 -> Execution time -> #### REMOVED ####
#columnName <- "Execution.time"
#tagsToConsider <- c("H","M","L","-")
#prettyPrintedTags <- c("H" = "High",
#                       "M" = "Medium",
#                       "L" = "Low",
#                       "-" = "NA")
#createPlot(columnName, tagsToConsider, prettyPrintedTags, FALSE, 8, "ExecutionTime", "Other", 10, 3)

######## RQ3 -> Number of eval apps
columnName <- "X..apps.evaluated"
tagsToConsider <- levels(as.factor(data[[columnName]]))
prettyPrintedTags <- c("H" = "High",
                       "M" = "Medium",
                       "L" = "Low")
createPlot(columnName, tagsToConsider, prettyPrintedTags, FALSE, 9, "AppsEvaluated", "Other", 10, 3)

######## RQ3 -> Apps provenance 
columnName <- "Toy.Real.apps"
tagsToConsider <- levels(as.factor(data[[columnName]]))
prettyPrintedTags <- c("T" = "Validation",
                       "R" = "Evaluation")
createPlot(columnName, tagsToConsider, prettyPrintedTags, FALSE, 10, "AppsProvenance", "Other", 10, 3)

######## RQ3 -> Evaluation.soundness
columnName <- "Evaluation.soundness"
tagsToConsider <- levels(as.factor(data[[columnName]]))
prettyPrintedTags <- c("Y" = "High",
                       "P" = "Medium",
                       "N" = "Low")
createPlot(columnName, tagsToConsider, prettyPrintedTags, FALSE, 8, "EvaluationSoundness", "Other", 10, 3)


