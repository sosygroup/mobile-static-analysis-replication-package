data <- read.csv("../rawData/data.csv", header = TRUE, quote = "\"", dec = ".", fill = TRUE, comment.char = "", na.strings = "-")

tablesPath <- "../../paper/tables/"

columnName <- "Additional.input"
filePath <- paste(tablesPath, columnName, ".tex", sep="")

tagsToConsider <- levels(data[[columnName]])

# se si vuole considerare solo un sottoinsieme dei tag (e.g., solo workshop papers e conference papers), 
# specificarli qui sotto e decommentare la riga. Tutto ci?? che non sta qui va nella categoria "Other"

#tagsToConsider <- c("API CALLS-PERMISSIONS MAPPING", "API DESCRIPTION", "BUG REPORTS", "LIST OF API CALLS OF INTEREST")

# qui ci vanno le coppie valore-label nel caso in cui vogliamo rinominare alcune label del parametro
prettyPrintedTags <- c("API CALLS-PERMISSIONS MAPPING"="API Calls-Permissions mapping",
                       "API DESCRIPTION" = "APIs description",
                       "BUG REPORTS" = "Bug reports",
                       "LIST OF API CALLS OF INTEREST" = "API calls of interest",
                       "PRIVACY POLICY" = "Privacy policy",
                       "ANDROID API MODELS" = "Android API models",
                       "ANDROID SOURCES AND SINKS" = "Android sources & sinks",
                       "API PRIVACY POLICY" = "API privacy policy",
                       "API-CONTEXT MAPPING" ="API-Context mapping",
                       "APP STORE DESCRIPTIONS" = "App Store Descriptions",
                       "APP STORE POLICY" = "App store policy",
                       "APP FLOW POLICY" = "App flow policy",
                       "BUG FIXING PATCHES" = "Bug Fixing Patches",
                       "CODE REVISIONS" = "Code revisions",
                       "CODE LINES-ENERGY CONSUMPTION MAPPING" = "Code lines-Energy Consumption mapping",
                       "EVENT TRACES" = "Event Traces",
                       "EXECUTION TRACES" = "Execution Traces",
                       "CPU PROFILE" = "Cpu profile",
                       "GOOGLE PLAY METADATA" = "Store Metadata",
                       "KNOWLEDGE BASE" = "Knowledge Base",
                       "LAYERS CONSTRAINS" = "Layers Constrains",
                       "LIST OF USER DEFINED CALLBACKS" = "User Defined Callbacks",
                       "APP DESCRIPTION" = "App description",
                       "USER DEFINED ANALYSIS" = "User defined Analysis",
                       "USER DEFINED FLOW POLICY" = "User defined flow policy",
                       "WORKLOAD DESCRIPTION " = "Workload Description",
                       "AOSP WITH ANALYSIS STUBS" = "AOSP implementation with analysis stubs",
                       "SOURCES AND SINKS LIST" = "Sources and sinks list",
                       "PERMISSION TO REVOKE" = "Target permission to revoke"
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





