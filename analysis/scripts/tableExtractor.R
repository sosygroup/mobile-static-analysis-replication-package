data <- read.csv("../rawData/data.csv", header = TRUE, quote = "\"", dec = ".", fill = TRUE, comment.char = "", na.strings = "-")

tablesPath <- "../../paper/tables/"

columnName <- "Analysis.type"
filePath <- paste(tablesPath, columnName, ".tex", sep="")

tagsToConsider <- levels(data[[columnName]])

# se si vuole considerare solo un sottoinsieme dei tag (e.g., solo workshop papers e conference papers), 
# specificarli qui sotto e decommentare la riga. Tutto ci?? che non sta qui va nella categoria "Other"

#tagsToConsider <- c("0", "1")

# qui ci vanno le coppie valore-label nel caso in cui vogliamo rinominare alcune label del parametro
prettyPrintedTags <- c("FLOW ANALYSIS" = "Flow Analysis",
                       "TAINT ANALYSIS" = "Taint Analysis",
                       "DATA MINING" = "Data Mining",
                       "CLASS ANALYSIS" = "Class Analysis",
                       "CLASSIFICATION" = "Classification",
                       "SLICING" = "Slicing",
                       "MODEL BASED" = "Model Based Analysis",
                       "STRING ANALYSIS" = "String Analysis",
                       "ABSTRACT INTERPRETATION" = "Abstract Interpretation",
                       "CONSTANT PROPAGATION" = "Constant Propagation",
                       "CODE INSTRUMENTATION" = "Code Instrumentation",
                       "TYPE INFERENCE" = "Type inference",
                       "SYMBOLIC EXECUTION" = "Symbolic Execution",
                       "POINTS-TO ANALYSIS" = "Pointer Analysis",
                       "NULLNESS ANALYSIS" = "Nullness Analysis",
                       "TERMINATION ANALYSIS" = "Termination Analysis",
                       "STATISTICAL ANALYSIS" = "Statistical Analysis",
                       "TYPESTATE ANALYSIS" = "Typestate Analysis",
                       "PATTERN-BASED CONTEXTUAL ANALYSIS" = "Pattern-based Analysis",
                       "RESPONSIVENESS ANALYSIS" = "Responsiveness Analysis"
)

idColumnName <- "UID"

othersSupported <- FALSE

# DO NOT CHANGE ANYTHING BELOW THIS LINE

separator <- ", "

tagsToConsider <- unlist(unique(as.list(strsplit(paste(tagsToConsider, sep=separator, collapse=separator), separator)[[1]])))

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
      print(prettyPrintedTags[[tags[i]]])
      cleanTags[i] <- prettyPrintedTags[[tags[i]]]
    }
  }
  cleanTags <<- gsub("\\_", " ", tags)
  tagNames <<- cleanTags
}

createTable(columnName,tagsToConsider)

getPrimaryList <- function(currentTag) {
  assigned <- subset(data, grepl(currentTag, data[[column]]))[[idColumnName]]
  unassigned <<- setdiff(unassigned, assigned)
  return(paste(paste(prefix, assigned, "", sep=""), collapse=separator))
}

getPrimaryOccurrences <- function(currentTag) {
  return(length(subset(data, grepl(currentTag, data[[column]]))[[idColumnName]]))
}

getOthersLine <- function() {
  otherList <- unassigned # c("1", "2", "3")
  otherListString <- paste(paste(prefix, otherList, "", sep=""), collapse=separator)
  return(paste("Other", " & \\databar{", length(otherList), "} & ", otherListString, "  \\\\ % OTHER \n", sep=""))
}

getTableLine <- function(currentTag, currentName, currentOccurrences) {
  currentList <- getPrimaryList(currentTag)
  return(paste(currentName, " & \\databar{", currentOccurrences, "} & ", currentList, "  \\\\ % ", currentTag, "\n", sep=""))
}

occurrences <- c()
unassigned <- data[[idColumnName]]

for(i in 1:length(tags)) {
  occurrences[i] <- getPrimaryOccurrences(tags[i])
}

combined <- data.frame(tags, tagNames, occurrences)
combined <- combined[order(combined$occurrences, decreasing=T),]

prefix <- "P"

# we print the table
result <- paste("
\\renewcommand{\\maxnum}{", (nrow(data) / 2) - (nrow(data) / 6), "}
\\begin{center}
\\begin{table}[h]
\\caption{", caption, "\\label{tab:", column, "}} {
\\centering
\\scriptsize {
\\begin{tabular}{| p{3cm} | l | p{6.5cm} | } 
\\hline
{\\bf ", columnName, "} & {\\bf \\#Studies} & {\\bf Studies} \\\\ \\hline", sep="")
for(i in 1:length(combined$tags)) {
  result <- paste(result, getTableLine(combined$tags[i], combined$tagNames[i], combined$occurrences[i]));
}
if(othersSupported && length(unassigned != 0)) {
  result <- paste(result, "\\hline", getOthersLine())
}
result <- paste(result, "\\hline
\\end{tabular}
}
}
\\end{table}
\\end{center}")

write(result, file=filePath)





