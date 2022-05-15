#setting library code
library(rtweet)
library(dplyr)
library(tidyverse)
library(tidytext)
library(stringr)
data(stop_words)
library(tidyr)

######################
## Electric Vehicle ##
######################
# collecting data from Twitter
EV_data <- search_tweets(
  "electric car", n=18000, include_rts = FALSE, lang ="en"
)

# filtering advertise & duplicated tweet
EV_clean <- EV_data %>%
              #subsetting showing data
            select('screen_name','text','source','favorite_count',
                   'retweet_count','hashtags') %>%
            
              # (assume having "http" as advertisement)
            filter(!str_detect(text, "https")) %>% 
            
              # eliminate duplicated tweet
            group_by(screen_name) %>%
            distinct(text, .keep_all =T) %>%
            
            ungroup()

# Deleting Numbers
library(tm)
EV_clean$text <- removeNumbers(EV_clean$text)

unnest_reg <- "([^A-Za-z_\\d#']|'(?![A-Za-z_\\d#]))"

# EV tokenization
EV_token <- EV_clean %>%
            unnest_tokens(word, text,
                          token = "regex", pattern = unnest_reg)%>%
            anti_join(stop_words)%>% #dropping stop words
            count(word, sort = T) 

#Token cleaning (word with no meaning or duplicated keywords)
EV_token <- EV_token %>%
            filter(!str_detect(word, "don")) %>%
            filter(!str_detect(word, "ev"))

# Add word proportion
EV_token_clean <- EV_token %>%
  mutate(word, n, proportion = (n/sum(n))*100)

# plotting top 25 words
library(ggplot2)
hist_EV_token <- EV_token_clean %>%
  filter(n<800 ) %>%
  top_n(25) %>%
  mutate(word = reorder(word,n )) %>%
  ggplot(aes(word, n))+
  geom_col()+
  geom_text(aes(label = comma(n, accuracy = 1)),
            hjust =1.03, col='white')+
  labs(title = "Top 25 words with Electric Vehicle",
       subtitle = "02.01.2021 ~ 02.10.2021",
       x = NULL)+
  coord_flip()

print(hist_EV_token)


####################
## Hybrid Vehicle ##
####################
# collecting data from Twitter
hybrid_data <- search_tweets(
  "hybrid car", n=18000, include_rts = FALSE, lang="en"
)

# filtering advertise & duplicated tweet
hybrid_clean <- hybrid_data %>%
                  
                  #subsetting showing data
                select('screen_name','text','source','favorite_count',
                       'retweet_count','hashtags') %>%
                 
                  # (assume having "http" as advertisement)
                filter(!str_detect(text, "https")) %>% 
  
                  # eliminate duplicated tweet
                group_by(screen_name) %>%
                distinct(text, .keep_all =T) %>%
                ungroup()

# dropping numbers in the text
hybrid_clean$text <- removeNumbers(hybrid_clean$text)

# Hybrid tokenization
hybrid_token <- hybrid_clean %>%
                unnest_tokens(word, text,
                              token = "regex", pattern = unnest_reg)%>%
                anti_join(stop_words)%>%
                count(word, sort = T) 

#Token cleaning (word with no meaning or duplicated keywords)
hybrid_token <- hybrid_token %>%
                filter(!str_detect(word, "car")) %>%
                filter(!str_detect(word, "ve"))

# plotting top 25 words
library(ggplot2)
hist_hybrid_token <- hybrid_token %>%
                     filter(n<400 ) %>%
                     top_n(25) %>%
                     mutate(word = reorder(word,n )) %>%
                     ggplot(aes(word, n))+
                     geom_col()+
                     geom_text(aes(label = comma(n, accuracy = 1)),
                               hjust =1.03, col='white')+
                     labs(title = "Top 25 words with Hybrid Vehicle",
                          subtitle = "02.01.2021 ~ 02.10.2021",
                          x = NULL)+
                    coord_flip()

print(hist_hybrid_token)

######################
## gasoline Vehicle ##
######################
# collecting data from Twitter
gasoline_data <- search_tweets(
  "gasoline car", n=18000, include_rts = FALSE, lang="en"
)

# filtering advertise & duplicated tweet
gas_clean <- gasoline_data %>%

               #subsetting showing data
             select('screen_name','text','source','favorite_count',
                    'retweet_count','hashtags') %>%

               # (assume having "http" as advertisement)
             filter(!str_detect(text, "https")) %>% 

               # eliminate duplicated tweet
             group_by(screen_name) %>%
             distinct(text, .keep_all =T) %>%
             ungroup()

gas_clean$text <- removeNumbers(gas_clean$text)

# Gasoline tokenization
gas_token <- gas_clean %>%
             unnest_tokens(word, text,
                           token = "regex", pattern = unnest_reg)%>%
             anti_join(stop_words)%>%
             count(word, sort = T) 

#########################
###### TD-IDF ###########
#########################
# gathering 3 categories data
full_df <- bind_rows(EV_clean %>%
                       mutate(text, category = 'EV'),
                     hybrid_clean%>%
                       mutate(text, category = 'hybrid'),
                     gas_clean %>%
                       mutate(text, category = 'gasoline'))

# tokenization
full_df_clean <- full_df %>%
                 unnest_tokens(word, text) %>%
                 anti_join(stop_words)%>%
                 count(category, word, sort = T) %>%
                 ungroup()


full_df_clean <- full_df_clean %>%
                 bind_tf_idf(word, category, n)

# tf-idf graphical approach EV
full_df_clean %>%
        arrange(desc(tf_idf)) %>%
        mutate(word=factor(word, levels =rev(unique(word)))) %>%
        group_by(category) %>%
        filter(category == 'EV') %>%
        filter(n<100) %>%
        top_n(15) %>%
        ungroup %>%
        ggplot(aes(word, tf_idf, fill=category))+
        geom_col(show.legend=FALSE)+
        labs(title = "Top 15 tf-idf words",
             subtitle = "02.01.2021 ~ 02.10.2021",
             x = NULL)+
        labs(x=NULL, y="tf-idf")+
        facet_wrap(~category, ncol=2, scales="free")+
        coord_flip()

# tf-idf graphical approach hybrid
full_df_clean %>%
        arrange(desc(tf_idf)) %>%
        mutate(word=factor(word, levels =rev(unique(word)))) %>%
        group_by(category) %>%
        filter(category == 'hybrid') %>%
        filter(n<100) %>%
        top_n(15) %>%
        ungroup %>%
        ggplot(aes(word, tf_idf, fill=category))+
        geom_col(show.legend=FALSE)+
        labs(title = "Top 15 tf-idf words",
             subtitle = "02.01.2021 ~ 02.10.2021",
             x = NULL)+
        labs(x=NULL, y="tf-idf")+
        facet_wrap(~category, ncol=2, scales="free")+
        coord_flip()

#############################
###### Sentiments ###########
#############################
# NRC sentiment graph
full_df_clean %>%
  inner_join(get_sentiments("nrc")) %>%
  filter(category == "EV") %>%
  count(sentiment, sort=TRUE) %>%
  mutate(sentiment = reorder(sentiment,n )) %>%
  mutate(proportion = n/sum(n)) %>%
  ggplot(aes(sentiment, n))+
  geom_col()+
  geom_text(aes(label = comma(n, accuracy = 1)),
            hjust =1.03, col='white')+
  labs(title = "EV nrc Sentiment",
       subtitle = "02.01.2021 ~ 02.10.2021",
       x = NULL)+
  coord_flip()


# BING sentiment graph
full_df_clean %>%
  inner_join(get_sentiments("bing")) %>%
  filter(category == "EV") %>%
  count(sentiment, sort=TRUE) %>%
  mutate(percentage = n/sum(n)) %>%
  mutate(sentiment = reorder(sentiment,n )) %>%
  ggplot(aes(sentiment, n))+
  geom_col()+
  geom_text(aes(label = comma(percentage)),
            hjust =1.03, col='white')+
  labs(title = "EV big Sentiment",
       subtitle = "02.01.2021 ~ 02.10.2021",
       x = NULL)+
  coord_flip()

#########################
###### N-gram ###########
#########################
# bigram
car_bigram <- full_df %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2) %>%
  count(bigram, sort = TRUE) %>%
  separate(bigram, c("word1", "word2"), sep = " ")

# exclude stop words
bigrams_filtered <- car_bigram %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) 

# Graph
library(igraph)
library(ggraph)

#use lower n for less data
bigram_graph <- bigrams_filtered %>%
  filter(n>17) %>%
  graph_from_data_frame()

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)


#############################
###### word cloud ###########
#############################

library(wordcloud)
library(reshape2)
cloud_df <- full_df %>%
  group_by(category) %>%
  
  unnest_tokens(word, text)%>%
  filter(category == "EV") %>%
  anti_join(stop_words) %>%
  count(word, sort=T)

cloud_df %>%
  inner_join(get_sentiments("nrc")) %>%
  mutate(percentage = n/sum(n)) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20","grey50"),
                   max.words=80, scale = c(1, 0.9))


#creating a sentiment word cloud for the bing library
library(reshape2)
cloud_df %>%
  inner_join(get_sentiments("bing")) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100, scale = c(1, 0.9))
