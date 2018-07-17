pacman::p_load(tidyverse)
Packs <- readRDS("2018_06_14_17_00page.rds")
Net <- read_csv("~/Crawl/2018_06_14_21_04NetworkCran.csv")
m <- matrix(0,nrow=length(Packs$Package),ncol=length(Packs$Package))
colnames(m) <- sort(Packs$Package)
rownames(m) <- sort(Packs$Package)

Net[1,1]

for(i in 1:nrow(Net)){
  try(m[as.character(Net[i,1]),as.character(Net[i,2])] <- 1)  
  message(paste(i, "of", nrow(Net), "Ready!!"))
}


# sorting in decreasing order of connectivity  
m <- as.data.frame(m)
m<-m %>% mutate(sum = rowSums(.[1:nrow(m)]))          
m<-m[order(m$sum, decreasing = T),]
m$sum<-NULL
m<-data.frame(t(m))
m<-m %>% mutate(sum = rowSums(.[1:nrow(m)]))          
m<-m[order(m$sum, decreasing = T),]
m$sum<-NULL
m<-t(m)
# plot heatmap
png("heatmap.png", res=600,height =  8, width = 8, units = "in" )
heatmap(m,na.rm = T, Rowv = NA, Colv = NA, col="red", scale="none", margins = c(8,8))
dev.off()
