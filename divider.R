main <- read.csv("utopia_corpus.csv")[c(-1)]

authors <- main %>% distinct(author)


data1 <- data.frame(text=character(), title=character(), author=character())
data2 <- data.frame(text=character(), title=character(), author=character())
data3 <- data.frame(text=character(), title=character(), author=character())


for (i in seq_along(authors[,1])) {
  data <- main %>%
    filter(author==authors[i,1])
  
  
  write(authors[i,1], stdout())
  
  write(as.integer(length(data[,1]) / 3), stdout())
  write(as.integer((length(data[,1]) * 2) / 3), stdout())
  write(length(data[,1]), stdout())
  
  data1 <- rbind(data[1:as.integer(length(data[,1]) / 3), ], data1)
  data2 <- rbind(data[(as.integer(length(data[,1]) / 3) + 1):(as.integer((length(data[,1]) * 2) / 3)), ], data2)
  data3 <- rbind(data[(as.integer((length(data[,1]) * 2) / 3) + 1) : (length(data[,1])), ], data3)
}


write.csv(data1, "utopia_seg1.csv")
write.csv(data2, "utopia_seg2.csv")
write.csv(data3, "utopia_seg3.csv")