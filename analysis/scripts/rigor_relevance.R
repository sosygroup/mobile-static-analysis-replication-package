# the file name of the pdf being produced
fileName <- "../../output/figures/Rigor.pdf"

# here we select which data frame is containing all the data
data <- read.delim("../../../data-filtered.txt", header = TRUE, quote = "\"", dec = ".", fill = TRUE, comment.char = "", na.strings = "N_A")

plotRigor <- function() {
  
  fileName <<- "../../output/figures/Rigor.pdf"
  pdf(fileName, width=6.5, height=3)
  par(mar=c(2, 5, 1, 1))
  par(mfrow=c(1, 1))
  par(las=1)

  data2 <<- structure(list(
    R1= c(length(subset(data$Rigor...context, data$Rigor...context==0)), length(subset(data$Rigor...context, data$Rigor...context==0.5)), length(subset(data$Rigor...context, data$Rigor...context==1))),
    R2= c(length(subset(data$Rigor...design, data$Rigor...design==0)), length(subset(data$Rigor...design, data$Rigor...design==0.5)), length(subset(data$Rigor...design, data$Rigor...design==1))),
    R3= c(length(subset(data$Rigor...validity, data$Rigor...validity==0)), length(subset(data$Rigor...validity, data$Rigor...validity==0.5)), length(subset(data$Rigor...validity, data$Rigor...validity==1)))), 
    .Names = c("Context described", "Study design described", "Validity discussed"), class = "data.frame", row.names = c(NA, -3))
  
  colours <<- gray.colors(3)#c("grey", "grey", "grey")
  barplot(as.matrix(data2), main="", ylim=c(0,60), ylab = "Scores frequency", cex.names = 1, cex.lab = 1, cex.main = 1, beside=TRUE, col=colours)
  legend(10.5,60,c("0","0.5","1"),fill=gray.colors(length(colours)))
  # context labels
  qvalue0 <<- length(subset(data$Rigor...context, data$Rigor...context==0))
  qvalue05 <<- length(subset(data$Rigor...context, data$Rigor...context==0.5))
  qvalue1 <<- length(subset(data$Rigor...context, data$Rigor...context==1))
  dist0 <<- 1.5
  dist05 <<- 2.5
  dist1 <<- 3.5
  text(x=c(dist0, dist05, dist1), y = c(qvalue0+3,qvalue05+3,qvalue1+3), label = c(qvalue0,qvalue05,qvalue1), col = "black")
  # design labels
  qvalue0 <<- length(subset(data$Rigor...design, data$Rigor...design==0))
  qvalue05 <<- length(subset(data$Rigor...design, data$Rigor...design==0.5))
  qvalue1 <<- length(subset(data$Rigor...design, data$Rigor...design==1))
  dist0 <<- dist0 + 4
  dist05 <<- dist05 + 4
  dist1 <<- dist1 + 4
  text(x=c(dist0, dist05, dist1), y = c(qvalue0+3,qvalue05+3,qvalue1+3), label = c(qvalue0,qvalue05,qvalue1), col = "black")
  # validity labels
  qvalue0 <<- length(subset(data$Rigor...validity, data$Rigor...validity==0))
  qvalue05 <<- length(subset(data$Rigor...validity, data$Rigor...validity==0.5))
  qvalue1 <<- length(subset(data$Rigor...validity, data$Rigor...validity==1))
  dist0 <<- dist0 + 4
  dist05 <<- dist05 + 4
  dist1 <<- dist1 + 4
  text(x=c(dist0, dist05, dist1), y = c(qvalue0+3,qvalue05+3,qvalue1+3), label = c(qvalue0,qvalue05,qvalue1), col = "black")
}

plotRelevance <- function() {
  fileName <<- "../../output/figures/Relevance.pdf"
  pdf(fileName, width=6.5, height=3)
  par(mar=c(2, 5, 1, 1))
  par(mfrow=c(1, 1))
  par(las=1)
  
  data3 <- structure(list(
  I1= c(length(subset(data$Industrial.relevance...subjects, data$Industrial.relevance...subjects==0)), length(subset(data$Industrial.relevance...subjects, data$Industrial.relevance...subjects==1))),
  I2= c(length(subset(data$Industrial.relevance...context, data$Industrial.relevance...context==0)), length(subset(data$Industrial.relevance...context, data$Industrial.relevance...context==1))),
  I3= c(length(subset(data$Industrial.relevance...scale, data$Industrial.relevance...scale==0)), length(subset(data$Industrial.relevance...scale, data$Industrial.relevance...scale==1))),
  I4= c(length(subset(data$Industrial.relevance...method, data$Industrial.relevance...method==0)), length(subset(data$Industrial.relevance...method, data$Industrial.relevance...method==1)))), 
  .Names = c("Subjects", "Context", "Scale", "Method"), class = "data.frame", row.names = c(NA, -2))
  
  colours <- gray.colors(2)
  barplot(as.matrix(data3), main="", ylim=c(0,60), ylab = "Scores frequency", cex.names = 1, cex.lab = 1, cex.main = 1, beside=TRUE, col=colours)
  legend(11.1,60,c("0","1"),fill=gray.colors(length(colours)))
  # subjects labels
  qvalue0 <<- length(subset(data$Industrial.relevance...subjects, data$Industrial.relevance...subjects==0))
  qvalue1 <<- length(subset(data$Industrial.relevance...subjects, data$Industrial.relevance...subjects==1))
  dist0 <<- 1.5
  dist1 <<- 2.5
  text(x=c(dist0, dist1), y = c(qvalue0+3,qvalue1+3), label = c(qvalue0,qvalue1), col = "black")
  # context labels
  qvalue0 <<- length(subset(data$Industrial.relevance...context, data$Industrial.relevance...context==0))
  qvalue1 <<- length(subset(data$Industrial.relevance...context, data$Industrial.relevance...context==1))
  dist0 <<- dist0 + 3
  dist1 <<- dist1 + 3
  text(x=c(dist0, dist1), y = c(qvalue0+3,qvalue1+3), label = c(qvalue0,qvalue1), col = "black")
  # scale labels
  qvalue0 <<- length(subset(data$Industrial.relevance...scale, data$Industrial.relevance...scale==0))
  qvalue1 <<- length(subset(data$Industrial.relevance...scale, data$Industrial.relevance...scale==1))
  dist0 <<- dist0 + 3
  dist1 <<- dist1 + 3
  text(x=c(dist0, dist1), y = c(qvalue0+3,qvalue1+3), label = c(qvalue0,qvalue1), col = "black")
  # method labels
  qvalue0 <<- length(subset(data$Industrial.relevance...method, data$Industrial.relevance...method==0))
  qvalue1 <<- length(subset(data$Industrial.relevance...method, data$Industrial.relevance...method==1))
  dist0 <<- dist0 + 3
  dist1 <<- dist1 + 3
  text(x=c(dist0, dist1), y = c(qvalue0+3,qvalue1+3), label = c(qvalue0,qvalue1), col = "black")
}

plotRigorDensity <- function() {
  fileName <<- "../../output/figures/RigorDensity.pdf"
  pdf(fileName, width=6.5, height=3)
  par(mar=c(4, 5, 1, 1))
  par(mfrow=c(1, 1))
  par(las=1)
  d <<- density(dataSource$RIGOR, bw=0.3) # returns the density data
  d$y <<- d$y * length(data$RIGOR)
  plot(d, xlim=c(0,3), ylim=c(0,58), xlab="Total rigor score", ylab="Frequency of rigor score",main="") # plots the results
  polygon(d, col="grey", border="black")
}

plotRelevanceDensity <- function() {
  fileName <<- "../../output/figures/RelevanceDensity.pdf"
  pdf(fileName, width=6.5, height=3)
  par(mar=c(4, 5, 1, 1))
  par(mfrow=c(1, 1))
  par(las=1)
  d <<- density(dataSource$IR, bw=0.4) # returns the density data
  d$y <<- d$y * length(data$IR)
  plot(d, xlim=c(0,4), ylim=c(0,58), xlab="Total industrial relevance score", ylab="Frequency of ind. relevance score",main="") # plots the results
  polygon(d, col="grey", border="black")
}

plotBubble <- function() {
  library(ggplot2)
  library(plyr)
  
  fileName <<- "../../output/figures/rigorRelevanceBubbles.pdf"
  # pdf(fileName, width=6.5, height=3)
  # par(mar=c(4, 5, 1, 1))
  # par(mfrow=c(1, 1))
  # par(las=1)
  
  bubble <- data.frame(data$RIGOR, data$IR)
  bubble <- head(bubble, -2)
  counts <- count(bubble, c("data.IR", "data.RIGOR"))
  #counts <- head(counts, -1)
  plot <- ggplot(counts, aes(x = data.IR, y = data.RIGOR, size = freq)) +
    geom_point(shape=21,color="black", fill="gray", alpha=0.9) +
    theme_classic() +
    theme(panel.grid.major = element_line(colour = "grey", size = .3)) + 
    geom_text(aes(label=counts$freq),size=3, alpha=1)+
    labs(x="Industrial relevance", y="Rigor") +
    scale_fill_gradient2(low="green", high="grey", limit=c(0,max(counts$freq)),name="") +
    scale_size_area(max_size = 25) +
    scale_y_continuous(breaks=round(seq(0, 3, by = 0.5),1)) + 
    theme(legend.position="none", axis.text.x=element_text(colour="black", size = 10),
          axis.text.y=element_text(colour="black", size = 10) )
  print(plot)
  ggsave(fileName, plot, width=6.5, height=5 , units="in", scale=1)
  plot
}

plotBubble()

#plotRigor()
#plotRelevance()

#plotRigorDensity()
#plotRelevanceDensity()



dev.off()

# library(irr)
# 
# # quality agreement checker
# 
# # remove all the rows with NAs
# data <- quality[!(is.na(quality$Q1_Fede)),]
# 
# attribute1 <- "Q1_Fede"
# attribute2 <- "Q1_Ivano"
# 
# tbl1 <- table(as.factor(data[[attribute1]]), as.factor(data[[attribute2]]))
# support1 <- data[, c(attribute1, attribute2)]
# 
# attribute1 <- "Q2_Fede"
# attribute2 <- "Q2_Ivano"
# 
# tbl2 <- table(as.factor(data[[attribute1]]), as.factor(data[[attribute2]]))
# support2 <- data[, c(attribute1, attribute2)]
# 
# attribute1 <- "Q3_Fede"
# attribute2 <- "Q3_Ivano"
# 
# tbl3 <- table(as.factor(data[[attribute1]]), as.factor(data[[attribute2]]))
# support3 <- data[, c(attribute1, attribute2)]
# 
# attribute1 <- "Q4_Fede"
# attribute2 <- "Q4_Ivano"
# 
# tbl4 <- table(as.factor(data[[attribute1]]), as.factor(data[[attribute2]]))
# support4 <- data[, c(attribute1, attribute2)]
# 
# attribute1 <- "Q5_Fede"
# attribute2 <- "Q5_Ivano"
# 
# tbl5 <- table(as.factor(data[[attribute1]]), as.factor(data[[attribute2]]))
# support5 <- data[, c(attribute1, attribute2)]
# 
# attribute1 <- "Q6_Fede"
# attribute2 <- "Q6_Ivano"
# 
# tbl6 <- table(as.factor(data[[attribute1]]), as.factor(data[[attribute2]]))
# support6 <- data[, c(attribute1, attribute2)]
# 
# # print(chisq.test(tbl1)$p.value)
# # print(kappa2(support1, "unweighted"))
# # print(mean(data[[attribute1]]))
# # print(mean(data[[attribute2]]))
# 
# print(tbl1)
# 
# scoresFede <- c()
# 
# index = 1
# for(i in 1:nrow(data)) {
#   row <- data[i,]
#   scoresFede <- append(scoresFede,  row$Q1_Fede + row$Q2_Fede + row$Q3_Fede + row$Q4_Fede + row$Q5_Fede + row$Q6_Fede)
#   index <- i
# }
# 
# # Now we plot the quality values
# 
# pdf("../../../journal_paper/figures/qualityTotal.pdf", width=10, height=7.5)
# par(mar=c(5, 5, 1, 1))
# par(mfrow=c(1, 1))
# par(las=1)
# 
# # Grouped Bar Plot
# data2 <- structure(list(
#   Q1= c(length(subset(data$Q1, data$Q1==0)), length(subset(data$Q1, data$Q1==0.5)), length(subset(data$Q1, data$Q1==1))), 
#   Q2= c(length(subset(data$Q2, data$Q2==0)), length(subset(data$Q2, data$Q2==0.5)), length(subset(data$Q2, data$Q2==1))), 
#   Q3= c(length(subset(data$Q3, data$Q3==0)), length(subset(data$Q3, data$Q3==0.5)), length(subset(data$Q3, data$Q3==1))), 
#   Q4= c(length(subset(data$Q4, data$Q4==0)), length(subset(data$Q4, data$Q4==0.5)), length(subset(data$Q4, data$Q4==1))), 
#   Q5= c(length(subset(data$Q5, data$Q5==0)), length(subset(data$Q5, data$Q5==0.5)), length(subset(data$Q5, data$Q5==1))), 
#   Q6= c(length(subset(data$Q6, data$Q6==0)), length(subset(data$Q6, data$Q6==0.5)), length(subset(data$Q6, data$Q6==1)))), .Names = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6"), class = "data.frame", row.names = c(NA, -3))
# colours <- gray.colors(3)#c("grey", "grey", "grey")
# 
# # barplot(as.matrix(data2), main="", ylim=c(0,50), ylab = "Quality scores frequency", cex.names = 1, cex.lab = 1, cex.main = 1, beside=TRUE, col=colours)
# # legend(21.6,50,c("0","0.5","1"),fill=gray.colors(3))
# # qvalue0 <- length(subset(data$Q1, data$Q1==0))
# # qvalue05 <- length(subset(data$Q1, data$Q1==0.5))
# # qvalue1 <- length(subset(data$Q1, data$Q1==1))
# # dist0 <- 1.5
# # dist05 <- 2.5
# # dist1 <- 3.5
# # text(x=c(dist0, dist05, dist1), y = c(qvalue0+1,qvalue05+1,qvalue1+1), label = c(qvalue0,qvalue05,qvalue1), col = "black")
# # qvalue0 <- length(subset(data$Q2, data$Q2==0))
# # qvalue05 <- length(subset(data$Q2, data$Q2==0.5))
# # qvalue1 <- length(subset(data$Q2, data$Q2==1))
# # dist0 <- dist0 + 4
# # dist05 <- dist05 + 4
# # dist1 <- dist1 + 4
# # text(x=c(dist0, dist05, dist1), y = c(qvalue0+1,qvalue05+1,qvalue1+1), label = c(qvalue0,qvalue05,qvalue1), col = "black")
# # qvalue0 <- length(subset(data$Q3, data$Q3==0))
# # qvalue05 <- length(subset(data$Q3, data$Q3==0.5))
# # qvalue1 <- length(subset(data$Q3, data$Q3==1))
# # dist0 <- dist0 + 4
# # dist05 <- dist05 + 4
# # dist1 <- dist1 + 4
# # text(x=c(dist0, dist05, dist1), y = c(qvalue0+1,qvalue05+1,qvalue1+1), label = c(qvalue0,qvalue05,qvalue1), col = "black")
# # qvalue0 <- length(subset(data$Q4, data$Q4==0))
# # qvalue05 <- length(subset(data$Q4, data$Q4==0.5))
# # qvalue1 <- length(subset(data$Q4, data$Q4==1))
# # dist0 <- dist0 + 4
# # dist05 <- dist05 + 4
# # dist1 <- dist1 + 4
# # text(x=c(dist0, dist05, dist1), y = c(qvalue0+1,qvalue05+1,qvalue1+1), label = c(qvalue0,qvalue05,qvalue1), col = "black")
# # qvalue0 <- length(subset(data$Q5, data$Q5==0))
# # qvalue05 <- length(subset(data$Q5, data$Q5==0.5))
# # qvalue1 <- length(subset(data$Q5, data$Q5==1))
# # dist0 <- dist0 + 4
# # dist05 <- dist05 + 4
# # dist1 <- dist1 + 4
# # text(x=c(dist0, dist05, dist1), y = c(qvalue0+1,qvalue05+1,qvalue1+1), label = c(qvalue0,qvalue05,qvalue1), col = "black")
# # qvalue0 <- length(subset(data$Q6, data$Q6==0))
# # qvalue05 <- length(subset(data$Q6, data$Q6==0.5))
# # qvalue1 <- length(subset(data$Q6, data$Q6==1))
# # dist0 <- dist0 + 4
# # dist05 <- dist05 + 4
# # dist1 <- dist1 + 4
# # text(x=c(dist0, dist05, dist1), y = c(qvalue0+1,qvalue05+1,qvalue1+1), label = c(qvalue0,qvalue05,qvalue1), col = "black")
# 
# 
# dev.off()
