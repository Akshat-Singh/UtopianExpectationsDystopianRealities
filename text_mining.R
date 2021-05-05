library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("syuzhet")
library("ggplot2")
library(tidytext)
library(dplyr)
library(tidyr)
library(scales)
library(ggthemes)
library(magrittr)
library(devtools)
library(stringr)
library(igraph)
library(ggraph)
library(widyr)


data(stop_words)

# Import clean csvs containing the texts
utopia_corpus <- read.csv('utopia_corpus.csv')[c(-1)]
dystopia_corpus <- read.csv('dystopia_corpus.csv')[c(-1)]


# Tidy the utopia corpus and remove the stoppers. Find the count
tidy_utopia <- utopia_corpus %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tidy_utopia_wordcount <- tidy_utopia %>%
  count(word, sort=TRUE)


# Repeat the charade with the dystopia corpus
tidy_dystopia <- dystopia_corpus %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tidy_dystopia_wordcount <- tidy_dystopia %>%
  count(word, sort=TRUE)



# Plot wordcount for utopia
tidy_utopia %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>% 
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  labs(title = "10 most frequent words in Corpus 1", 
       caption = "This plot was made with the Tidyverse austen_books data set",
       x = "Words",
       y = "Occurences in the Corpus")+
  theme_light()+
  coord_flip()


# Do the same for dystopia
tidy_dystopia %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>% 
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  labs(title = "10 most frequent words in Corpus 1", 
       caption = "This plot was made with the Tidyverse austen_books data set",
       x = "Words",
       y = "Occurences in the Corpus")+
  theme_light()+
  coord_flip()




################################################################################
################################################################################
################################################################################
################################################################################
################################################################################

utopia_tdm <- TermDocumentMatrix(Corpus(VectorSource(utopia_corpus$text))) 
  
utopia_corr <- findAssocs(utopia_tdm, terms = c("liberty", "equality", "freedom"), corlimit = 0.25 )

utopia_corr

dystopia_tdm <- TermDocumentMatrix(Corpus(VectorSource(dystopia_corpus$text))) 
  
dystopia_corr <- findAssocs(dystopia_tdm, terms = c("liberty", "equality", "freedom"), corlimit = 0.5 )

dystopia_corr


nice_correlations_utopia <- findAssocs(utopia_tdm, terms = findFreqTerms(utopia_tdm, lowfreq = 20), corlimit = 0.50)
nice_correlations_utopia

nice_correlations_dystopia <- findAssocs(dystopia_tdm, terms = findFreqTerms(dystopia_tdm, lowfreq = 100), corlimit = 0.50)
nice_correlations_dystopia
  

################################################################################
################################################################################
################################################################################
################################################################################
################################################################################

bigrams_utopia <- utopia_corpus %>%
  unnest_tokens(bigram, text, token="ngrams", n = 2)

bigrams_utopia_nostop <- bigrams_utopia %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word) %>%
  count(word1, word2, sort = TRUE) %>%
  na.omit() %>%
  unite("bigram", c(word1, word2), sep = " ")


# Visualize bigrams_utopia
bigrams_utopia_nostop %>%
  top_n(10) %>% 
  mutate(bigram = reorder(bigram, n)) %>%
  ggplot(aes(bigram, n)) +
  geom_col() +
  labs(title = "Top Bigrams in the Utopia Corpus", 
       x = "Top Bigrams",
       y = "Frequency")+
  theme_light()+
  coord_flip()



bigrams_dystopia <- dystopia_corpus %>%
  unnest_tokens(bigram, text, token="ngrams", n = 2)

bigrams_dystopia_nostop <- bigrams_dystopia %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word) %>%
  count(word1, word2, sort = TRUE) %>%
  na.omit() %>%
  unite("bigram", c(word1, word2), sep = " ")


# Visualize bigrams_utopia
bigrams_dystopia_nostop %>%
  top_n(10) %>% 
  mutate(bigram = reorder(bigram, n)) %>%
  ggplot(aes(bigram, n)) +
  geom_col() +
  labs(title = "Top Bigrams in the Dystopia Corpus", 
       x = "Top Bigrams",
       y = "Frequency")+
  theme_light()+
  coord_flip()



# Enhanced Correlation
utopia_corr <- utopia_corpus %>%
  mutate(section=row_number() %/% 10) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word)

utopia_word_pairs <- utopia_corr %>%
  pairwise_count(word, section, sort = TRUE)


utopia_word_pairs %>%
  filter(item1=="glory")

dystopia_corr <- dystopia_corpus %>%
  mutate(section=row_number() %/% 10) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word)

dystopia_word_pairs <- dystopia_corr %>%
  pairwise_count(word, section, sort = TRUE)


dystopia_word_pairs %>%
  filter(item1=="glory")


################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################

utopia_corr <- utopia_corpus %>%
  mutate(section=row_number() %/% 10) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word)


utopia_correlation_pairs <- utopia_corr %>%
  pairwise_count(word, section, sort=TRUE)

utopia_correlation_pairs %>%
  filter(item1 == "love")


dystopia_corr <- dystopia_corpus %>%
  mutate(section=row_number() %/% 10) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word)


dystopia_correlation_pairs <- dystopia_corr %>%
  pairwise_count(word, section, sort=TRUE)

dystopia_correlation_pairs %>%
  filter(item1 == "love")




utopia_correlation_real <- utopia_corr %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, section, sort=TRUE) %>%
  filter(!is.na(correlation))





dystopia_correlation_real <- dystopia_corr %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, section, sort=TRUE) %>%
  filter(!is.na(correlation), correlation > 0.5)

dystopia_corr %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, section, sort=TRUE) %>%
  filter(!is.na(correlation), correlation > 0.5) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()


utopia_corr %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, section, sort=TRUE) %>%
  filter(!is.na(correlation), correlation > 0.4) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()

dystopia_correlation_real
