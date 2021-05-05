
author <- c()
title <- c()
text <- c()

anthem <- read.table('anthem.txt', sep='\r', header=T)
for (i in 1:(nrow(anthem) / 3)) {
  text <- append(text, anthem[i,])
  author <- append(author, "Rand, Ayn")
  title <- append(title, "Anthem")
}

temp_corpus <- data.frame(text, title, author)

# gutbooks <- gutenberg_download(c(32, 2130), mirror="http://mirrors.xmission.com/gutenberg/", meta_fields = c("author","title"))[c(-1)]

dystopia_corpus <- rbind(dystopia_corpus, temp_corpus)


write.csv(dystopia_corpus, "dystopia_corpus.csv")
