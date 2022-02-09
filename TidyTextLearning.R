# Package names
packages <- c("dplyr", "janeaustenr", "tidytext", "textdata", "wesanderson", "forcats", "plotly", "gutenbergr", "stringr", "tidyr")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))


book_words <- austen_books() %>%
  unnest_tokens(word, text) %>%
  count(book, word, sort = TRUE)

total_words <- book_words %>% 
  group_by(book) %>% 
  summarize(total = sum(n))

book_words <- left_join(book_words, total_words)

book_words


ggplot(book_words, aes(n/total, fill = book)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~book, ncol = 2, scales = "free_y")

freq_by_rank <- book_words %>% 
  group_by(book) %>% #already sorted in order of desc freq
  mutate(rank = row_number(), 
         `term frequency` = n/total) %>%
  ungroup()

freq_by_rank

freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = book)) + 
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()

freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = book)) + 
  geom_abline(intercept = -0.62, slope = -1.1, 
              color = "gray50", linetype = 2) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()

book_tf_idf <- book_words %>%
  bind_tf_idf(word, book, n)

book_tf_idf %>%
  select(-total) %>%
  arrange(desc(tf_idf))


p <- book_tf_idf %>%
  group_by(book) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL) +
  #scale_fill_manual(values=wes_palette(n=6, name="GrandBudapest1")) 
  scale_fill_manual(values = wes_palette(6, name = "Zissou1", type = "continuous"))

ggplotly(p)

#################

darwin <- gutenberg_works(author == "Darwin, Charles")

darwin_books <- gutenberg_download(c(944, 1227, 2087, 2010), 
                              meta_fields = "title")

colnames(darwin_books) <- stringr::str_replace_all(colnames(darwin_books),"[:punct:]"," ")

darwin_words <- darwin_books %>%
  unnest_tokens(word, text) %>%
  count(title, word, sort = TRUE)

plot_darwin <- darwin_words %>%
  bind_tf_idf(word, title, n) %>%
  mutate(title = factor(
    title,
    levels =
      c(
        "The Voyage of the Beagle",
        "The Expression of the Emotions in Man and Animals",
        "Life and Letters of Charles Darwin — Volume 1",
        "The Autobiography of Charles Darwin"
      )
  ))

plot_darwin %>% 
  group_by(title) %>% 
  slice_max(tf_idf, n = 15) %>% 
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(tf_idf, word, fill = title)) +
  geom_col(show.legend = FALSE) +
  labs(x = "tf-idf", y = NULL) +
  facet_wrap(~title, ncol = 2, scales = "free") +
  scale_fill_manual(values=wes_palette(n=4, name="GrandBudapest1")) 

library(stringr)

darwin_books %>% 
  filter(str_detect(text, "eyebrows")) %>% 
  select(text)

##########################################

# N-grams!

austen_bigrams <- austen_books() %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

austen_bigrams

austen_bigrams %>%
  count(bigram, sort = TRUE)

# Separate into 2 words

bigrams_separated <- austen_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

# Filter out  stop words

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) # stop words here is 1000+ words

# Count: new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

bigram_counts

# “separate/filter/count/unite” let us find the most common bigrams not containing stop-words.

# Unite: bring them back together!
bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

bigrams_united

# Tri-grams!
# Note that when using count you lose the grouping (group_by) variable, here book

austen_books() %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>%
  count(word1, word2, word3, sort = TRUE)

# Analyze

# most common mentions of street in books?

bigrams_filtered %>%
  filter(word2 == "street") %>%
  count(book, word1, sort = TRUE)

# tf-idf
# 
# Another approach is to look at a term’s inverse document frequency (idf), 
# which decreases the weight for commonly used words and increases the weight for words that are not used very much in a collection of documents. This can be combined with term frequency to calculate a term’s tf-idf (the two quantities multiplied together), 
# the frequency of a term adjusted for how rarely it is used.
# idf decreases in accordance to how common a word is among a corpus of documents

bigram_tf_idf <- bigrams_united %>%
  count(book, bigram) %>%
  bind_tf_idf(bigram, book, n) %>%
  arrange(desc(tf_idf))

bigram_tf_idf

# plot

a <- bigram_tf_idf %>%
  group_by(book) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(bigram, tf_idf), fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL) +
  #scale_fill_manual(values=wes_palette(n=6, name="GrandBudapest1")) 
  scale_fill_manual(values = wes_palette(6, name = "Zissou1", type = "continuous"))

ggplotly(a)

# Sentiment analysis with negations

bigrams_separated %>%
  filter(word1 == "not") %>%
  count(word1, word2, sort = TRUE)

# how often sentiment-associated words are preceded by “not” or other negating words.

# gives a numeric sentiment value for each word, 
#with positive or negative numbers indicating the direction of the sentiment.

library(tidytext)

AFINN <- get_sentiments("afinn")

AFINN

not_words <- bigrams_separated %>%
  filter(word1 == "not") %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word2, value, sort = TRUE) #summarizes by word2 to give word, sentiment value, and freq

# multiply their value by the number of times they appear 
#(so that a word with a value of +3 occurring 10 times has as much impact as a 
# word with a sentiment value of +1 occurring 30 times). We visualize the result with a bar plot

library(ggplot2)

not_words %>%
  mutate(contribution = n * value) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(n * value, word2, fill = n * value > 0)) +
  geom_col(show.legend = FALSE) +
  labs(x = "Sentiment value * number of occurrences",
       y = "Words preceded by \"not\"") +
  scale_fill_manual(values = rev(wes_palette(6, name = "Zissou1", type = "continuous")))


# why not replace n+ value with "contribution" ?
# same result

not_words %>%
  mutate(contribution = n * value) %>%
  arrange(desc(abs(contribution))) %>% #sort by absolute value
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(contribution, word2, fill = contribution > 0)) + #splits contribution into + and -
  geom_col(show.legend = FALSE) +
  labs(x = "Sentiment value * number of occurrences",
       y = "Words preceded by \"not\"") +
  scale_fill_manual(values = rev(wes_palette(6, name = "Zissou1", type = "continuous")))


#####

# Visualizing a network of bigrams with ggraph
library(igraph)

# original counts
bigram_counts

# filter for only relatively common combinations
bigram_graph <- bigram_counts %>%
  filter(n > 20) %>%
  graph_from_data_frame()

# graph_from_data_frame() function takes a data frame of edges with columns 
#for “from”, “to”, and edge attributes (in this case n):

bigram_graph

# for a basic graph we need to add three layers: nodes, edges, and text.

library(ggraph)
set.seed(2017)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

# Figure 4.4: Common bigrams in Jane Austen’s novels, 
#showing those that occurred more than 20 times and where neither word was a stop word

# Prettier and more informative
# add directionality with an arrow,

set.seed(2020)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = T,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "plum3", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()
#########################
# Formula for visualizing bigrams in any text!

# libraries

library(dplyr)
library(tidyr)
library(tidytext)
library(ggplot2)
library(igraph)
library(ggraph)

# create bigrams function

count_bigrams <- function(dataset) {
  dataset %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word) %>%
    count(word1, word2, sort = TRUE)
}

# visualizing bigrams function

visualize_bigrams <- function(bigrams) {
  set.seed(2016)
  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
  
  bigrams %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
    geom_node_point(color = "plum3", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()
}

#######################

# the King James version is book 10 on Project Gutenberg:
library(gutenbergr)
kjv <- gutenberg_download(10)

library(stringr)

kjv_bigrams <- kjv %>%
  count_bigrams() # function created above

# filter out rare combinations, as well as digits
kjv_bigrams %>%
  filter(n > 40,
         !str_detect(word1, "\\d"),
         !str_detect(word2, "\\d")) %>%
  visualize_bigrams() # function created above

####
# Counting and correlating pairs of words with the widyr package
# Most operations for finding pairwise counts or correlations 
# need to turn the data into a wide matrix first.

# What words tend to appear within the same section?

austen_section_words <- austen_books() %>%
  filter(book == "Pride & Prejudice") %>%
  mutate(section = row_number() %/% 10) %>%
  filter(section > 0) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word)

austen_section_words

library(widyr)

# count words co-occuring within sections
word_pairs <- austen_section_words %>%
  pairwise_count(word, section, sort = TRUE)

word_pairs

# The pairwise_cor() function in widyr lets 
# us find the phi coefficient between words based on how often they appear in the same section.

word_cors <- austen_section_words %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, section, sort = TRUE)

word_cors

# Words from Pride and Prejudice that 
# were most correlated with ‘elizabeth’, ‘pounds’, ‘married’, and ‘pride’

word_cors %>%
  filter(item1 %in% c("elizabeth", "pounds", "married", "pride")) %>%
  group_by(item1) %>%
  slice_max(correlation, n = 6) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item1, scales = "free") +
  coord_flip()

# bigrams

set.seed(2016)

word_cors %>%
  filter(correlation > .15) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "plum3", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()

####
#
# Cosine and Jaccard similarities Check with Darwin and any other 
# Edge network analysis

=======


# Package names
packages <- c("dplyr", "janeaustenr", "tidytext", "textdata", "wesanderson", "forcats", "plotly", "gutenbergr", "stringr", "tidyr")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))


book_words <- austen_books() %>%
  unnest_tokens(word, text) %>%
  count(book, word, sort = TRUE)

total_words <- book_words %>% 
  group_by(book) %>% 
  summarize(total = sum(n))

book_words <- left_join(book_words, total_words)

book_words


ggplot(book_words, aes(n/total, fill = book)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~book, ncol = 2, scales = "free_y")

freq_by_rank <- book_words %>% 
  group_by(book) %>% #already sorted in order of desc freq
  mutate(rank = row_number(), 
         `term frequency` = n/total) %>%
  ungroup()

freq_by_rank

freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = book)) + 
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()

freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = book)) + 
  geom_abline(intercept = -0.62, slope = -1.1, 
              color = "gray50", linetype = 2) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()

book_tf_idf <- book_words %>%
  bind_tf_idf(word, book, n)

book_tf_idf %>%
  select(-total) %>%
  arrange(desc(tf_idf))


p <- book_tf_idf %>%
  group_by(book) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL) +
  #scale_fill_manual(values=wes_palette(n=6, name="GrandBudapest1")) 
  scale_fill_manual(values = wes_palette(6, name = "Zissou1", type = "continuous"))

ggplotly(p)

#################

darwin <- gutenberg_works(author == "Darwin, Charles")

darwin_books <- gutenberg_download(c(944, 1227, 2087, 2010), 
                              meta_fields = "title")

colnames(darwin_books) <- stringr::str_replace_all(colnames(darwin_books),"[:punct:]"," ")

darwin_words <- darwin_books %>%
  unnest_tokens(word, text) %>%
  count(title, word, sort = TRUE)

plot_darwin <- darwin_words %>%
  bind_tf_idf(word, title, n) %>%
  mutate(title = factor(
    title,
    levels =
      c(
        "The Voyage of the Beagle",
        "The Expression of the Emotions in Man and Animals",
        "Life and Letters of Charles Darwin — Volume 1",
        "The Autobiography of Charles Darwin"
      )
  ))

plot_darwin %>% 
  group_by(title) %>% 
  slice_max(tf_idf, n = 15) %>% 
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(tf_idf, word, fill = title)) +
  geom_col(show.legend = FALSE) +
  labs(x = "tf-idf", y = NULL) +
  facet_wrap(~title, ncol = 2, scales = "free") +
  scale_fill_manual(values=wes_palette(n=4, name="GrandBudapest1")) 

library(stringr)

darwin_books %>% 
  filter(str_detect(text, "eyebrows")) %>% 
  select(text)

##########################################

# N-grams!

austen_bigrams <- austen_books() %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

austen_bigrams

austen_bigrams %>%
  count(bigram, sort = TRUE)

# Separate into 2 words

bigrams_separated <- austen_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

# Filter out  stop words

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) # stop words here is 1000+ words

# Count: new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

bigram_counts

# “separate/filter/count/unite” let us find the most common bigrams not containing stop-words.

# Unite: bring them back together!
bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

bigrams_united

# Tri-grams!
# Note that when using count you lose the grouping (group_by) variable, here book

austen_books() %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>%
  count(word1, word2, word3, sort = TRUE)

# Analyze

# most common mentions of street in books?

bigrams_filtered %>%
  filter(word2 == "street") %>%
  count(book, word1, sort = TRUE)

# tf-idf
# 
# Another approach is to look at a term’s inverse document frequency (idf), 
# which decreases the weight for commonly used words and increases the weight for words that are not used very much in a collection of documents. This can be combined with term frequency to calculate a term’s tf-idf (the two quantities multiplied together), 
# the frequency of a term adjusted for how rarely it is used.
# idf decreases in accordance to how common a word is among a corpus of documents

bigram_tf_idf <- bigrams_united %>%
  count(book, bigram) %>%
  bind_tf_idf(bigram, book, n) %>%
  arrange(desc(tf_idf))

bigram_tf_idf

# plot

a <- bigram_tf_idf %>%
  group_by(book) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(bigram, tf_idf), fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL) +
  #scale_fill_manual(values=wes_palette(n=6, name="GrandBudapest1")) 
  scale_fill_manual(values = wes_palette(6, name = "Zissou1", type = "continuous"))

ggplotly(a)

# Sentiment analysis with negations

bigrams_separated %>%
  filter(word1 == "not") %>%
  count(word1, word2, sort = TRUE)

# how often sentiment-associated words are preceded by “not” or other negating words.

# gives a numeric sentiment value for each word, 
#with positive or negative numbers indicating the direction of the sentiment.

library(tidytext)

AFINN <- get_sentiments("afinn")

AFINN

not_words <- bigrams_separated %>%
  filter(word1 == "not") %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word2, value, sort = TRUE) #summarizes by word2 to give word, sentiment value, and freq

# multiply their value by the number of times they appear 
#(so that a word with a value of +3 occurring 10 times has as much impact as a 
# word with a sentiment value of +1 occurring 30 times). We visualize the result with a bar plot

library(ggplot2)

not_words %>%
  mutate(contribution = n * value) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(n * value, word2, fill = n * value > 0)) +
  geom_col(show.legend = FALSE) +
  labs(x = "Sentiment value * number of occurrences",
       y = "Words preceded by \"not\"") +
  scale_fill_manual(values = rev(wes_palette(6, name = "Zissou1", type = "continuous")))


# why not replace n+ value with "contribution" ?
# same result

not_words %>%
  mutate(contribution = n * value) %>%
  arrange(desc(abs(contribution))) %>% #sort by absolute value
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(contribution, word2, fill = contribution > 0)) + #splits contribution into + and -
  geom_col(show.legend = FALSE) +
  labs(x = "Sentiment value * number of occurrences",
       y = "Words preceded by \"not\"") +
  scale_fill_manual(values = rev(wes_palette(6, name = "Zissou1", type = "continuous")))


#####

# Visualizing a network of bigrams with ggraph
library(igraph)

# original counts
bigram_counts

# filter for only relatively common combinations
bigram_graph <- bigram_counts %>%
  filter(n > 20) %>%
  graph_from_data_frame()

# graph_from_data_frame() function takes a data frame of edges with columns 
#for “from”, “to”, and edge attributes (in this case n):

bigram_graph

# for a basic graph we need to add three layers: nodes, edges, and text.

library(ggraph)
set.seed(2017)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

# Figure 4.4: Common bigrams in Jane Austen’s novels, 
#showing those that occurred more than 20 times and where neither word was a stop word

# Prettier and more informative
# add directionality with an arrow,

set.seed(2020)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = T,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "plum3", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()
#########################
# Formula for visualizing bigrams in any text!

# libraries

library(dplyr)
library(tidyr)
library(tidytext)
library(ggplot2)
library(igraph)
library(ggraph)

# create bigrams function

count_bigrams <- function(dataset) {
  dataset %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word) %>%
    count(word1, word2, sort = TRUE)
}

# visualizing bigrams function

visualize_bigrams <- function(bigrams) {
  set.seed(2016)
  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
  
  bigrams %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
    geom_node_point(color = "plum3", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()
}

#######################

# the King James version is book 10 on Project Gutenberg:
library(gutenbergr)
kjv <- gutenberg_download(10)

library(stringr)

kjv_bigrams <- kjv %>%
  count_bigrams() # function created above

# filter out rare combinations, as well as digits
kjv_bigrams %>%
  filter(n > 40,
         !str_detect(word1, "\\d"),
         !str_detect(word2, "\\d")) %>%
  visualize_bigrams() # function created above

####
# Counting and correlating pairs of words with the widyr package
# Most operations for finding pairwise counts or correlations 
# need to turn the data into a wide matrix first.

# What words tend to appear within the same section?

austen_section_words <- austen_books() %>%
  filter(book == "Pride & Prejudice") %>%
  mutate(section = row_number() %/% 10) %>%
  filter(section > 0) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word)

austen_section_words

library(widyr)

# count words co-occuring within sections
word_pairs <- austen_section_words %>%
  pairwise_count(word, section, sort = TRUE)

word_pairs

# The pairwise_cor() function in widyr lets 
# us find the phi coefficient between words based on how often they appear in the same section.

word_cors <- austen_section_words %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, section, sort = TRUE)

word_cors

# Words from Pride and Prejudice that 
# were most correlated with ‘elizabeth’, ‘pounds’, ‘married’, and ‘pride’

word_cors %>%
  filter(item1 %in% c("elizabeth", "pounds", "married", "pride")) %>%
  group_by(item1) %>%
  slice_max(correlation, n = 6) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item1, scales = "free") +
  coord_flip()

# bigrams

set.seed(2016)

word_cors %>%
  filter(correlation > .15) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "plum3", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()

####
#
# Cosine and Jaccard similarities Check with Darwin and any other 
# Edge network analysis

>>>>>>> 7530a3320fb1bb1b0a160421a48cfd7c8d7d5db7
####