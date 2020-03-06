data <- read.csv("../rawData/data.csv", header = TRUE, quote = "\"", dec = ".", fill = TRUE, comment.char = "", na.strings = "-")

Var1ColumnName <- "Year.of.publication"
Var2ColumnName <- "Publication.venue.type"
fileName <- "../../paper/figures/pubTypes_year.pdf"
Var1Label <- ""
Var2Label <- ""
bubbleScaler <- 1.1

Var2ColumnOldValues <- c("Workshop", "Journal", "Conference")
Var2ColumnNewValues <- c("W", "J", "C")

#######################################################################

countOccurrences <- function(el1, el2) {
  sum <- 0
  for(i in 1:length(Elements1)) {
      if((Elements1[i] == el1) && (Elements2[i] == el2)) {
        sum <- sum + 1
      }
  }
  return(sum)
}

Elements1 <- as.character(data[[Var1ColumnName]])
Elements1 <- Elements1[!is.na(Elements1)]
#Var1 <- unique(Elements1)
Var1 <- c(min(data[[Var1ColumnName]]):max(data[[Var1ColumnName]]))#c("2006","2007","2008","2009","2010", "2011","2012","2013","2014", "2015")

Elements2 <- as.character(data[[Var2ColumnName]])
Elements2 <- Elements2[!is.na(Elements2)]
length(Elements2) <- length(Elements1)

for(i in 1:length(Elements2)) {
  for(j in 1:length(Var2ColumnOldValues)) {
    if(Elements2[i] == Var2ColumnOldValues[j]) {
      Elements2[i] <- Var2ColumnNewValues[j]
    }
  }
}

Var2 <- unique(Elements2)


#count <- rep(1, 7*4)
count <- vector("integer", length(Var1) * length(Var2))

grid <- data.frame(Var1, Var2, count)
grid <- grid[ order(grid[,1], grid[,2]), ]

print(grid[ order(grid[,1], grid[,2]), ])


index <- 0
for(i in 0:(length(Var1) - 1)) {
  index <- 3 * i
  for(j in 1:length(Var2)) {
    if(!is.na(grid[index + j, 1])) {
      grid[index + j, 3] <- countOccurrences(grid[index + j, 1], grid[index + j, 2])
    }
  }
}

# we remove all the rows with 0 value as count
grid <- grid[grid$count > 0, ]

# here we start creating the plot

library(ggplot2)
library(cowplot)
grid$radius <- sqrt( grid$count / pi ) * bubbleScaler

p <- ggplot(grid,aes(Var1,Var2))+
  #geom_point(aes(size=radius*10),shape=21,fill="white", alpha=0.8)+
  geom_point(data=grid,aes(fill=count,size=radius*13),shape=21,color="black", alpha=0.9) +
  scale_fill_gradient2(low="green", high="grey", limit=c(0,max(grid$count)),name=Var2Label) +
  geom_text(aes(label=count),size=5, alpha=1)+
  guides(fill=FALSE)+
  #geom_text(aes(label=count),size=5, alpha=1,vjust=3,hjust=-2)+
  scale_size_identity()+
  theme_cowplot() +
  theme(panel.grid.major = element_line(colour = "grey")) + 
  labs(x=Var1Label, y=Var2Label) +
  theme(axis.text.x = element_text(size=15, angle=90, hjust=1, vjust=0), axis.text.y = element_text(size=15),
  axis.title.x = element_text(size=15), axis.title.y = element_text(size=15)) + 
  theme(axis.line = element_line(color = 'black')) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 7)) +
  scale_y_discrete(labels = c("Conference","Journal","Workshop"))
  

print(p)
ggsave(fileName, p, width=3.4, height=1.5, units="in", scale=3)

