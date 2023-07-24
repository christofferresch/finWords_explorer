
############################################
############## FINANCE WORDS ############3##



require(rvest)
require(dplyr)
require(ggplot2)
require(ggthemes)
require(tibble)
require(SentimentAnalysis)
require(tm)
require(slam)
require(udpipe)
require(tidytext)
require(wordcloud)
require(textrank)
library(stringr)

link_e24 <- "https://e24.no"
page_e24 <- read_html(link_e24)

# Load in titles from e24
titles_e24 <- page_e24 %>% 
  html_nodes(".title") %>% 
  html_text()
titles_e24


# Load in finansavisen
link_fa <- "https://www.finansavisen.no/"
page_fa <- read_html(link_fa)

# Load in titles from finansavisen
titles_fa <- page_fa %>%  
  html_nodes("p") %>% 
  html_text() %>%
  as_tibble() %>%
  distinct() %>%
  unlist()
titles_fa

# Loading in borsen dagbladet website
link_bø <- "https://borsen.dagbladet.no/"
page_bø <- read_html(link_bø)

# Loading in titles from borsen Dagbladet website
titles_bø <- page_bø %>%  
  html_nodes("h3") %>% 
  html_text() %>%
  as_tibble() %>%
  slice(2:(n() - 2)) %>%
  distinct() %>%
  unlist()
titles_bø 


# Dagens Næringsliv
link_dn <- "https://www.dn.no/"
page_dn <- read_html(link_dn)

titles_dn <- page_dn %>%  
  html_nodes("p") %>% 
  html_text() %>%
  as_tibble() %>%
  distinct() %>%
  unlist()
titles_dn 

# Load the norwegian udpipe model
library(udpipe)
#tagger <- udpipe_download_model("norwegian-bokmaal")
udpipe_download_model("norwegian-bokmaal")
tagger <- udpipe_load_model("norwegian-bokmaal-ud-2.5-191206.udpipe")

# Create a udpipe dataframe of finasavisen
words_fa<- 
  udpipe_annotate(object = tagger,
                  x = titles_fa) %>% 
  as_tibble()


# Create a udpipe dataframe of e24
words_e24 <- udpipe_annotate(object = tagger,
                            x = titles_e24) %>% 
  as_tibble()

# Creating a udpipe dataframe for borsen dagbladet
words_bø <- udpipe_annotate(object = tagger,
                                        x = titles_bø) %>% 
  as_tibble()

# Creating a udpipe dataframe for dagens næringsliv
words_dn <- udpipe_annotate( object = tagger,
                                          x = titles_dn) %>% 
  as_tibble()


# Combine all news journals to dataframe
df_all <- rbind(words_fa,
            words_e24, 
            words_bø, 
            words_dn)


# Select sentence_id and sentence
#sentences <- df_all %>% 
#  select("doc_id","sentence_id","sentence") %>% 
#  unique() %>%
#  tolower()


# Function to count special characters in a string
count_special_chars <- function(text) {
  special_chars <- str_extract_all(text, "[^[:alnum:]]")
  sum(lengths(special_chars))
}



# Selecting sentence_id and lemma
terms <- df_all %>% 
  mutate(special_chars_count = sapply(lemma, count_special_chars)
  ) %>%
  filter(special_chars_count < 1) %>%
  filter(upos %in% c("NOUN","ADJ", "PROPN")) %>% 
  select("sentence_id","lemma") 

# Change lemmas tolower
#terms$lemma <- tolower(terms$lemma)

# Counting lemma
term_count <- terms %>% 
  count(terms$lemma)

# Arranging in descending order
term_count %>% 
  arrange(-term_count$n) -> term_count

# Filtering out words with less than 3 characthers
term_count %>% 
  filter(nchar(`terms$lemma`) > 4) -> term_count


require(RColorBrewer)
require(wordcloud2)
pal2 <- brewer.pal(6,"Dark2")

# Daily wordcloud for 4 financial newsjournals in Norway
wordcloud(words = term_count$`terms$lemma`,
          freq = term_count$n,
          min.freq = 2,
          max.words = 100,
          col= pal2,
          scale = c(3,.25),
          random.order = FALSE)
