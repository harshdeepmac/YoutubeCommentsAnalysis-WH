library(tidyverse)
library(devtools)
library(sentimentr)
library(stringr)
library(repurrrsive)
library(purrr)
library(stringr)
library(tidytext) 
library(wordcloud)
library(textdata)

comments <- read_csv('commentsfinal.csv')

uni_tokens <- comments %>%
  unnest_tokens(output = tokens , input = textDisplay)

uni_tokens  %>%
  count(tokens, sort = T)

sw<-get_stopwords()

cleaned_tokens_uni<- uni_tokens %>%
  filter(!tokens %in% sw$word)

cleaned_tokens_uni  %>%
  count(tokens, sort = T)

rare <- cleaned_tokens_uni %>%
  count(tokens, sort = T) %>%
  filter(n<10) %>%
  dplyr::select(tokens) %>%
  unique()

cleaned_tokens_uni <- cleaned_tokens_uni %>%
  filter(!tokens %in% rare$tokens)

al <- brewer.pal(8,"Dark2")
# plot the 100 most common words
cleaned_tokens_uni %>%   
  count(tokens) %>%  
  with(wordcloud(tokens, n, random.order = FALSE, max.words = 100, colors=al))

sentiment <- cleaned_tokens_uni  %>%
  left_join(get_sentiments("nrc") , by=c('tokens'='word')) %>%
  rename(nrc=sentiment) %>%
  left_join(get_sentiments("bing"), by=c('tokens'='word')) %>%
  rename(bing=sentiment) %>%
  left_join(get_sentiments("afinn"), by=c('tokens'='word')) %>%
  rename(afinn=value)

bing_sentiment <- sentiment %>%
  filter(!is.na(bing)) %>%
  count(tokens,bing, sort = T)

sentiment %>%
  filter(!is.na(bing)) %>%
  count(tokens) %>%
  with(wordcloud(tokens, n, random.order = FALSE, max.words = 50, colors=al))

bing_sentiment %>%
  filter(n>1000) %>%
  mutate(n=ifelse(bing=='negative', -n,n)) %>%
  mutate(tokens = reorder(tokens,n)) %>%
  ggplot(aes(tokens,n,fill=bing))+
  geom_col() +
  coord_flip()

nrc_sentiment <- sentiment %>%
  filter(!is.na(nrc)) %>%
  count(tokens,nrc, sort = T)

high_comment_video <- comments %>%
  count(videoId, sort = T) %>%
  filter(n>1300) %>%
  dplyr::select(videoId)

plot_senti <- sentiment %>%
  filter(videoId == high_comment_video$videoId) %>%
  filter(!is.na(nrc)) %>%
  filter(!is.na(bing)) %>%
  ggplot( aes(x=nrc)) +
  geom_bar() +
  facet_wrap(~videoId, nrow = 4,ncol = 2) +
  theme_bw()

plot_senti+theme_plots

