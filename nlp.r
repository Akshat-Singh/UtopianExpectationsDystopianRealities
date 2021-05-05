#NLP Libraries
library(rJava)
library(openNLP)
library(NLP)
library(syuzhet)

#Tidy data manipulation
library(stringr)
library(dplyr)
library(tidyr)
library(tidytext)
library(readr)
library(stringi)

#Corpus ingest
library(gutenbergr)

#Helper library
library(fuzzyjoin)
library(sqldf)

#Graphics library
library(ggiraphExtra)
library(ggplot2)
library(RColorBrewer)
library(scales)
library(tidygraph)



#set pipeline
wordAnnotator <- Maxent_Word_Token_Annotator(language = "en")
sentenceAnnotator <- Maxent_Sent_Token_Annotator(language = "en")
characterAnnotatorEN <- Maxent_Entity_Annotator(language = "en", kind = "person")
locationAnnotatorEN <- Maxent_Entity_Annotator(language = "en", kind = "location")

pipeline <- list(sentenceAnnotator,
                 wordAnnotator,
                 characterAnnotatorEN,
                 locationAnnotatorEN)


# Tool Number 2 #

utopia1_characters <- read.csv("utopia_seg1.csv")[c(-1)] %>%
  filter(kind=="person")

utopia2_characters <- read.csv("utopia_seg2.csv")[c(-1)] %>%
  filter(kind=="person")

utopia3_characters <- read.csv("utopia_seg3.csv")[c(-1)] %>%
  filter(kind=="person")



dystopia1_characters <- read.csv("dystopia_seg1.csv")[c(-1)] %>%
  filter(kind=="person")

dystopia2_characters <- read.csv("dystopia_seg2.csv")[c(-1)] %>%
  filter(kind=="person")

dystopia3_characters <- read.csv("dystopia_seg3.csv")[c(-1)] %>%
  filter(kind=="person")




dystopia_characters <- read.csv("dystopia_entities_clean.csv")[c(-1)] %>%
  filter(kind=="person")




utopia1_sentences <- read.csv("utopia_seg1.csv")[c(-1)] %>%
  filter(type == "sentence") %>%
  mutate(sentence_nr = row_number()) %>%
  select(author, title, words, sentence_nr) 

utopia2_sentences <- read.csv("utopia_seg2.csv")[c(-1)] %>%
  filter(type == "sentence") %>%
  mutate(sentence_nr = row_number()) %>%
  select(author, title, words, sentence_nr) 

utopia3_sentences <- read.csv("utopia_seg3.csv")[c(-1)] %>%
  filter(type == "sentence") %>%
  mutate(sentence_nr = row_number()) %>%
  select(author, title, words, sentence_nr) 



dystopia1_sentences <- read.csv("dystopia_seg1.csv")[c(-1)] %>%
  filter(type == "sentence") %>%
  mutate(sentence_nr = row_number()) %>%
  select(author, title, words, sentence_nr) 

dystopia2_sentences <- read.csv("dystopia_seg2.csv")[c(-1)] %>%
  filter(type == "sentence") %>%
  mutate(sentence_nr = row_number()) %>%
  select(author, title, words, sentence_nr) 

dystopia3_sentences <- read.csv("dystopia_seg3.csv")[c(-1)] %>%
  filter(type == "sentence") %>%
  mutate(sentence_nr = row_number()) %>%
  select(author, title, words, sentence_nr) 





dystopia_sentences <- read.csv("dystopia_corpus_df.csv")[c(-1)] %>%
  filter(type == "sentence") %>%
  mutate(sentence_nr = row_number()) %>%
  select(author, title, words, sentence_nr) 


pre_join_utopia1 <- utopia1_characters %>%  
  select(words, kind) %>% 
  drop_na()  %>% 
  distinct()

pre_join_utopia2 <- utopia2_characters %>%  
  select(words, kind) %>% 
  drop_na()  %>% 
  distinct()

pre_join_utopia3 <- utopia3_characters %>%  
  select(words, kind) %>% 
  drop_na()  %>% 
  distinct()


utopia_chunk_chars <- rbind(utopia1_characters, utopia2_characters)
utopia_chunk_chars <- rbind(utopia_chunk_chars, utopia3_characters)

dystopia_chunk_chars <- rbind(dystopia1_characters, dystopia2_characters)
dystopia_chunk_chars <- rbind(dystopia_chunk_chars, dystopia3_characters)




pre_join_utopia <- utopia_chunk_chars %>%  
  select(words, kind) %>% 
  drop_na()  %>% 
  distinct()

pre_join_dystopia <- dystopia_chunk_chars %>%  
  select(words, kind) %>% 
  drop_na()  %>% 
  distinct()




pre_join_dystopia <- dystopia_characters %>%  
  select(words, kind) %>% 
  drop_na()  %>% 
  distinct()




full_join_utopia1 <- fuzzy_join(utopia1_sentences, pre_join_utopia, match_fun = stri_detect_regex, by = "words", mode = "inner")

full_join_utopia1 <- full_join_utopia1 %>% 
  distinct()

full_join_utopia2 <- fuzzy_join(utopia2_sentences, pre_join_utopia, match_fun = stri_detect_regex, by = "words", mode = "inner")

full_join_utopia2 <- full_join_utopia2 %>% 
  distinct()

full_join_utopia3 <- fuzzy_join(utopia3_sentences, pre_join_utopia, match_fun = stri_detect_regex, by = "words", mode = "inner")

full_join_utopia3 <- full_join_utopia3 %>% 
  distinct()




full_join_dystopia1 <- fuzzy_join(dystopia1_sentences, pre_join_dystopia, match_fun = stri_detect_regex, by = "words", mode = "inner")

full_join_dystopia1 <- full_join_dystopia1 %>% 
  distinct()

full_join_dystopia2 <- fuzzy_join(dystopia2_sentences, pre_join_dystopia, match_fun = stri_detect_regex, by = "words", mode = "inner")

full_join_dystopia2 <- full_join_dystopia2 %>% 
  distinct()

full_join_dystopia3 <- fuzzy_join(dystopia3_sentences, pre_join_dystopia, match_fun = stri_detect_regex, by = "words", mode = "inner")

full_join_dystopia3 <- full_join_dystopia3 %>% 
  distinct()




full_join_dystopia <- fuzzy_join(dystopia_sentences, pre_join_dystopia, match_fun = stri_detect_regex, by = "words", mode = "inner")

full_join_dystopia <- full_join_dystopia %>% 
  distinct()





utopia1_fuzzyjoined <-  full_join_utopia1 %>%
  rename_at(.vars = vars(ends_with(".x")),
            .funs = funs(sub("[.]x$", "", .))) %>%
  rename(entity = words.y)

utopia2_fuzzyjoined <-  full_join_utopia2 %>%
  rename_at(.vars = vars(ends_with(".x")),
            .funs = funs(sub("[.]x$", "", .))) %>%
  rename(entity = words.y)

utopia3_fuzzyjoined <-  full_join_utopia3 %>%
  rename_at(.vars = vars(ends_with(".x")),
            .funs = funs(sub("[.]x$", "", .))) %>%
  rename(entity = words.y)




dystopia1_fuzzyjoined <-  full_join_dystopia1 %>%
  rename_at(.vars = vars(ends_with(".x")),
            .funs = funs(sub("[.]x$", "", .))) %>%
  rename(entity = words.y)

dystopia2_fuzzyjoined <-  full_join_dystopia2 %>%
  rename_at(.vars = vars(ends_with(".x")),
            .funs = funs(sub("[.]x$", "", .))) %>%
  rename(entity = words.y)

dystopia3_fuzzyjoined <-  full_join_dystopia3 %>%
  rename_at(.vars = vars(ends_with(".x")),
            .funs = funs(sub("[.]x$", "", .))) %>%
  rename(entity = words.y)






dystopia_fuzzyjoined <-  full_join_dystopia %>%
  rename_at(.vars = vars(ends_with(".x")),
            .funs = funs(sub("[.]x$", "", .))) %>%
  rename(entity = words.y)





full_join_dystopia <- dystopia_fuzzyjoined %>%
  unnest_tokens(word, words)



full_join_utopia1 <- utopia1_fuzzyjoined %>%
  unnest_tokens(word, words)

full_join_utopia2 <- utopia2_fuzzyjoined %>%
  unnest_tokens(word, words)

full_join_utopia3 <- utopia3_fuzzyjoined %>%
  unnest_tokens(word, words)



full_join_dystopia1 <- dystopia1_fuzzyjoined %>%
  unnest_tokens(word, words)

full_join_dystopia2 <- dystopia2_fuzzyjoined %>%
  unnest_tokens(word, words)

full_join_dystopia3 <- dystopia3_fuzzyjoined %>%
  unnest_tokens(word, words)





utopia1_char_sentiment <- full_join_utopia1 %>%
  group_by(author, title) %>%
  inner_join(get_sentiments("nrc")) %>% 
  count(sentence_nr, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

utopia2_char_sentiment <- full_join_utopia2 %>%
  group_by(author, title) %>%
  inner_join(get_sentiments("nrc")) %>% 
  count(sentence_nr, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

utopia3_char_sentiment <- full_join_utopia3 %>%
  group_by(author, title) %>%
  inner_join(get_sentiments("nrc")) %>% 
  count(sentence_nr, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)


dystopia1_char_sentiment <- full_join_dystopia1 %>%
  group_by(author, title) %>%
  inner_join(get_sentiments("nrc")) %>% 
  count(sentence_nr, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

dystopia2_char_sentiment <- full_join_dystopia2 %>%
  group_by(author, title) %>%
  inner_join(get_sentiments("nrc")) %>% 
  count(sentence_nr, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

dystopia3_char_sentiment <- full_join_dystopia3 %>%
  group_by(author, title) %>%
  inner_join(get_sentiments("nrc")) %>% 
  count(sentence_nr, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)





dystopia_char_sentiment <- full_join_dystopia %>%
  group_by(author, title) %>%
  inner_join(get_sentiments("nrc")) %>% 
  count(sentence_nr, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)


utopia1_charmatch_sentiment <- full_join_utopia1 %>%
  inner_join(utopia1_char_sentiment) %>%
  distinct_at(vars(-word))

utopia2_charmatch_sentiment <- full_join_utopia2 %>%
  inner_join(utopia2_char_sentiment) %>%
  distinct_at(vars(-word))

utopia3_charmatch_sentiment <- full_join_utopia3 %>%
  inner_join(utopia3_char_sentiment) %>%
  distinct_at(vars(-word))

dystopia1_charmatch_sentiment <- full_join_dystopia1 %>%
  inner_join(dystopia1_char_sentiment) %>%
  distinct_at(vars(-word))

dystopia2_charmatch_sentiment <- full_join_dystopia2 %>%
  inner_join(dystopia2_char_sentiment) %>%
  distinct_at(vars(-word))

dystopia3_charmatch_sentiment <- full_join_dystopia3 %>%
  inner_join(dystopia3_char_sentiment) %>%
  distinct_at(vars(-word))


### Clean up before going forward
write.csv(utopia1_charmatch_sentiment, "utopia_section1_emotion_dirty.csv")
write.csv(utopia2_charmatch_sentiment, "utopia_section2_emotion_dirty.csv")
write.csv(utopia3_charmatch_sentiment, "utopia_section3_emotion_dirty.csv")


write.csv(dystopia1_charmatch_sentiment, "dystopia_section1_emotion_dirty.csv")
write.csv(dystopia2_charmatch_sentiment, "dystopia_section2_emotion_dirty.csv")
write.csv(dystopia3_charmatch_sentiment, "dystopia_section3_emotion_dirty.csv")



### Re-read
utopia1_charmatch_sentiment <- read.csv("utopia_section1_emotion_clean.csv")
utopia2_charmatch_sentiment <- read.csv("utopia_section2_emotion_clean.csv")
utopia3_charmatch_sentiment <- read.csv("utopia_section3_emotion_clean.csv")


dystopia1_charmatch_sentiment <- read.csv("dystopia_section1_emotion_clean.csv")
dystopia2_charmatch_sentiment <- read.csv("dystopia_section2_emotion_clean.csv")
dystopia3_charmatch_sentiment <- read.csv("dystopia_section3_emotion_clean.csv")




dystopia_charmatch_sentiment <- full_join_dystopia %>%
  inner_join(dystopia_char_sentiment) %>%
  distinct_at(vars(-word))




utopia1_total <- utopia1_charmatch_sentiment %>%
  group_by(author, title, entity, kind) %>%
  summarise(total = mean(sentiment))

utopia2_total <- utopia2_charmatch_sentiment %>%
  group_by(author, title, entity, kind) %>%
  summarise(total = mean(sentiment))

utopia3_total <- utopia3_charmatch_sentiment %>%
  group_by(author, title, entity, kind) %>%
  summarise(total = mean(sentiment))



dystopia1_total <- dystopia1_charmatch_sentiment %>%
  group_by(author, title, entity, kind) %>%
  summarise(total = mean(sentiment))

dystopia2_total <- dystopia2_charmatch_sentiment %>%
  group_by(author, title, entity, kind) %>%
  summarise(total = mean(sentiment))

dystopia3_total <- dystopia3_charmatch_sentiment %>%
  group_by(author, title, entity, kind) %>%
  summarise(total = mean(sentiment))





dystopia_total <- dystopia_charmatch_sentiment %>%
  group_by(author, title, entity, kind) %>%
  summarise(total = mean(sentiment))



write.csv(dystopia_total, "dystopia_entities_sentiments.csv")


write.csv(utopia1_total, "utopia1_entities_sentiments.csv")
write.csv(utopia2_total, "utopia2_entities_sentiments.csv")
write.csv(utopia3_total, "utopia3_entities_sentiments.csv")

write.csv(dystopia1_total, "dystopia1_entities_sentiments.csv")
write.csv(dystopia2_total, "dystopia2_entities_sentiments.csv")
write.csv(dystopia3_total, "dystopia3_entities_sentiments.csv")






dystopia_total <- read.csv("dystopia_entities_sentiments_clean.csv")
utopia_total <- read.csv("utopia_entities_sentiments_clean.csv")


utopia1_total <- read.csv("utopia1_entities_sentiments.csv")
utopia2_total <- read.csv("utopia2_entities_sentiments.csv")
utopia3_total <- read.csv("utopia3_entities_sentiments.csv")


dystopia1_total <- read.csv("dystopia1_entities_sentiments.csv")
dystopia2_total <- read.csv("dystopia2_entities_sentiments.csv")
dystopia3_total <- read.csv("dystopia3_entities_sentiments.csv")



utopia1_total %>%
  group_by(title) %>%
  filter(kind == "person") %>%
  top_n(10) %>% 
  mutate(entity = reorder(entity, total)) %>% 
  ggplot(aes(entity, y = total, fill = title)) +
  geom_col() +
  facet_wrap( ~ title, scales = "free") +
  coord_flip()

utopia2_total %>%
  group_by(title) %>%
  filter(kind == "person") %>%
  top_n(10) %>% 
  mutate(entity = reorder(entity, total)) %>% 
  ggplot(aes(entity, y = total, fill = title)) +
  geom_col() +
  facet_wrap( ~ title, scales = "free") +
  coord_flip()

utopia3_total %>%
  group_by(title) %>%
  filter(kind == "person") %>%
  top_n(10) %>% 
  mutate(entity = reorder(entity, total)) %>% 
  ggplot(aes(entity, y = total, fill = title)) +
  geom_col() +
  facet_wrap( ~ title, scales = "free") +
  coord_flip()



dystopia1_total %>%
  group_by(title) %>%
  filter(kind == "person") %>%
  top_n(-9) %>% 
  mutate(entity = reorder(entity, total)) %>% 
  ggplot(aes(entity, y = total, fill = title)) +
  geom_col() +
  facet_wrap( ~ title, scales = "free") +
  coord_flip()

dystopia2_total %>%
  group_by(title) %>%
  filter(kind == "person") %>%
  top_n(-10) %>% 
  mutate(entity = reorder(entity, total)) %>% 
  ggplot(aes(entity, y = total, fill = title)) +
  geom_col() +
  facet_wrap( ~ title, scales = "free") +
  coord_flip()

dystopia3_total %>%
  group_by(title) %>%
  filter(kind == "person") %>%
  top_n(-10) %>% 
  mutate(entity = reorder(entity, total)) %>% 
  ggplot(aes(entity, y = total, fill = title)) +
  geom_col() +
  facet_wrap( ~ title, scales = "free") +
  coord_flip()

dystopia1_total %>%
  group_by(title) %>%
  filter(kind == "person") %>%
  top_n(10) %>% 
  mutate(entity = reorder(entity, total)) %>% 
  ggplot(aes(entity, y = total, fill = title)) +
  geom_col() +
  facet_wrap( ~ title, scales = "free") +
  coord_flip()

dystopia2_total %>%
  group_by(title) %>%
  filter(kind == "person") %>%
  top_n(10) %>% 
  mutate(entity = reorder(entity, total)) %>% 
  ggplot(aes(entity, y = total, fill = title)) +
  geom_col() +
  facet_wrap( ~ title, scales = "free") +
  coord_flip()

dystopia3_total %>%
  group_by(title) %>%
  filter(kind == "person") %>%
  top_n(10) %>% 
  mutate(entity = reorder(entity, total)) %>% 
  ggplot(aes(entity, y = total, fill = title)) +
  geom_col() +
  facet_wrap( ~ title, scales = "free") +
  coord_flip()



dystopia3_total %>%
  group_by(title) %>%
  summarise(total = log(sum(total))) %>%
  ggplot(aes(x = title, y = total)) + 
  geom_col(fill="#bbbb00")


dystopia2_total %>%
  group_by(title) %>%
  summarise(total = log(sum(total))) %>%
  ggplot(aes(x = title, y = total)) + 
  geom_col(fill="#7f00ff")


dystopia1_total %>%
  group_by(title) %>%
  summarise(total = log(sum(total))) %>%
  ggplot(aes(x = title, y = total)) + 
  geom_col(fill="#feb582")



utopia3_total %>%
  group_by(title) %>%
  summarise(total = log(sum(total))) %>%
  ggplot(aes(x = title, y = total)) + 
  geom_col(fill="#bbbb00")


utopia2_total %>%
  group_by(title) %>%
  summarise(total = log(sum(total))) %>%
  ggplot(aes(x = title, y = total)) + 
  geom_col(fill="#7f00ff")


utopia1_total %>%
  group_by(title) %>%
  summarise(total = log(sum(total))) %>%
  ggplot(aes(x = title, y = total)) + 
  geom_col(fill="#feb582")






dystopia_total %>%
  group_by(title) %>%
  filter(kind == "person") %>%
  top_n(10) %>% 
  mutate(entity = reorder(entity, total)) %>% 
  ggplot(aes(entity, y = total, fill = title)) +
  geom_col() +
  facet_wrap( ~ title, scales = "free") +
  coord_flip()




########### Bring out a network showing the variations
authors <- dystopia_characters %>%
  distinct(author)

vertices <- c()
edgeList <- c()



for (i in seq_along(authors[,1])) {
  author <- authors[i,1]
  author_characters <- filter(dystopia_total, author==authors[i,]) 
  

  vertices <- append(vertices, as.vector(author_characters$entity))
  
  for (j in seq_along(author_characters)) {
    edgeList <- append(edgeList, c(author, author_characters[j, 4]))
    
    if (is.na(edgeList[length(edgeList)])) {
      edgeList <- head(edgeList, -2)
    }
    else {
      if(author_characters[j, 7] == "m")
        colors <- append(colors, c("#AAAAAA", "#0000FF"))
      else
        colors <- append(colors, c("#AAAAAA", "#FFB6C1"))
    }
  }
  
}


edgeList
edgeList <- edgeList[!is.na(edgeList)]
edgeList
make_graph(edges=edgeList)
newgraph <- graph(edgeList, directed=FALSE)

colors <- c()
agg_sentiment <- c()
for (i in seq_along(V(newgraph)$name)) {
  character_gender <- dystopia_total %>%
    filter(entity == V(newgraph)$name[i])
  if (length(character_gender[, 1]) == 0)
    colors <- append(colors, "#000000")
  else if(character_gender[1, "gender"] == "m")
    colors <- append(colors, "#0000FF")
  else 
    colors <- append(colors, "#D90166")
  
  
  if (length(character_gender[, 1]) == 0)
    agg_sentiment <- append(agg_sentiment, "#AAAAAA")
  else if(character_gender[1, "total"] < 0)
    agg_sentiment <- append(agg_sentiment, "#FFFCBB")
  else 
    agg_sentiment <- append(agg_sentiment, "#FF7F7F")
  
}




plot(newgraph, vertex.color=agg_sentiment, vertex.label.dist = 1, vertex.label.color=colors)



########### Bring out a network showing the variations
authors <- utopia_characters %>%
  distinct(author)

vertices <- c()
edgeList <- c()



for (i in seq_along(authors[,1])) {
  author <- authors[i,1]
  author_characters <- filter(utopia_total, author==authors[i,]) 
  

  vertices <- append(vertices, as.vector(author_characters$entity))
  
  for (j in seq_along(author_characters)) {
    edgeList <- append(edgeList, c(author, author_characters[j, 4]))
    
    if (is.na(edgeList[length(edgeList)])) {
      edgeList <- head(edgeList, -2)
    }
    else {
      if(author_characters[j, 7] == "m")
        colors <- append(colors, c("#AAAAAA", "#0000FF"))
      else
        colors <- append(colors, c("#AAAAAA", "#FFB6C1"))
    }
  }
  
}


edgeList
edgeList <- edgeList[!is.na(edgeList)]
edgeList
make_graph(edges=edgeList)
newgraph <- graph(edgeList, directed=FALSE)

colors <- c()
agg_sentiment <- c()
for (i in seq_along(V(newgraph)$name)) {
  character_gender <- utopia_total %>%
    filter(entity == V(newgraph)$name[i])
  if (length(character_gender[, 1]) == 0)
    colors <- append(colors, "#000000")
  else if(character_gender[1, "gender"] == "m")
    colors <- append(colors, "#0000FF")
  else 
    colors <- append(colors, "#D90166")
  
  
  if (length(character_gender[, 1]) == 0)
    agg_sentiment <- append(agg_sentiment, "#AAAAAA")
  else if(character_gender[1, "total"] < 0)
    agg_sentiment <- append(agg_sentiment, "#FFFCBB")
  else 
    agg_sentiment <- append(agg_sentiment, "#FF7F7F")
  
}

  


plot(newgraph, vertex.color=agg_sentiment, vertex.label.dist = 1, vertex.label.color=colors)




