rm(list = ls())
library(text2vec)
library(tidyverse)
library(skimr)
library(dplyr)
library(recipes)
library(h2o)
library(ggplot2)
library(gridExtra)
library(tidyr)
library(dplyr)
library(tidytext)
library(scales)

titles <- read_csv('Title_updated.csv')
stats <- read_csv('All_Stats.csv')

tokens <- titles %>%
  unnest_tokens(output = word,input = Title)

sw = get_stopwords()

cleaned_tokens <- tokens %>%  
  filter(!word %in% sw$word)

cleaned_tokens %>%
  count(word, sort = T) %>%
  rename(word_freq = n) %>%
  ggplot(aes(x=word_freq)) +
    geom_histogram(aes(y=..count..), color="black", fill="blue", alpha=0.3) +
    scale_x_continuous(breaks=c(0:5,10,100,500,10e3), trans="log1p", expand=c(0,0)) +
    scale_y_continuous(breaks=c(0,100,1000,5e3,10e3,5e4,10e4,4e4), expand=c(0,0)) +
    theme_bw()
  
rare <- cleaned_tokens %>%
  count(word) %>%
  filter(n<10) %>%
  dplyr::select(word) %>%
  unique()

cleaned_tokens <- cleaned_tokens %>%
  filter(!word %in% rare$word)

word_counts_by_doc_id <- cleaned_tokens %>%
  group_by(X1) %>%
  count(word, sort = TRUE)

review_dtm <- word_counts_by_doc_id %>%  
  cast_dtm(X1, word, n)

tfidf <- word_counts_by_doc_id %>%  
  bind_tf_idf(word, X1, n) 

dtm_df <- tidy(review_dtm)

model_features_dtm <- dtm_df %>%
  spread(key=term, value = count,fill = 0)

model_features_tfidf <- tfidf %>%
  dplyr::select(c('X1','word','tf_idf')) %>%
  spread(key=word, value = tf_idf,fill = 0)

data<- model_features_tfidf %>%
  inner_join(stats, by=c('X1'='X1'))

data<- as.data.frame(data %>%
  dplyr::select(-c('likeCount','dislikeCount','favoriteCount','commentCount','id','X1')))
set.seed(1234)
train_subset_ids<-sample(c(1:dim(data)[1]), dim(data)[1]*0.7)  
train_data <- data[train_subset_ids,2:196]
test_data <- data[-train_subset_ids,2:196]

h2o.init()

train_h2o <- as.h2o(train_data, destination_frame = 'train.hex')
test_h2o <- as.h2o(test_data, destination_frame = 'test.hex')

splits <- h2o.splitFrame(
  data = train_h2o, ratios = 0.7, 
  seed = 1234)

train_data_h20 <- splits[[1]]
valid_data_h20 <-splits[[2]]

y <- 'viewCount'
x <- setdiff(names(train_data_h20),y)

model_3 <- h2o.deeplearning(
  x=x,y=y,
  training_frame = train_data_h20,
  validation_frame = valid_data_h20,
  model_id = 'NN_1',
  hidden = c(160,100,50),
  epochs = 25,
  distribution = 'gaussian',
  seed = 1234
)
#?h2o.deeplearning
#summary(model_3)
plot(model_3)

predict_train <- h2o.predict(model_3,train_h2o)
prediction_train <- as.vector(predict_train)
actual_train <- train_data$viewCount
cor(prediction_train,actual_train)^2

#h2o.performance(model_3, newdata = test_h2o)

predict_test <- h2o.predict(model_3,test_h2o)
prediction_test <- as.vector(predict_test)
actual_test <- as.vector(test_data$viewCount)
cor(prediction_test, actual_test)^2

#Neural Network with 3 hidden layers 160,100,50 epochs 100 dis=gaussian was giving max at 
#54 an =d 27 resp

##################################################

model_4 <- h2o.gbm(
  x=x,y=y,
  training_frame = train_data_h20,
  validation_frame = valid_data_h20,
  distribution = 'tweedie',
  max_depth = 15,
  ntrees =75
)
# tweedie 10,55--73,42
# posion 8,57 -- 74,50
plot(model_4)

predict_train <- h2o.predict(model_4,train_h2o)
prediction_train <- as.vector(predict_train)
actual_train <- train_data$viewCount
cor(prediction_train,actual_train)^2


predict_test <- h2o.predict(model_4,test_h2o)
prediction_test <- as.vector(predict_test)
actual_test <- as.vector(test_data$viewCount)
cor(prediction_test, actual_test)^2


#########################

#Recommendation 1
#Adding the number of words in the model

video_word_count <- tokens %>%
  group_by(X) %>%
  count(X)

titles$title_chr_count <- map_int(titles$Title,~nchar(.))

plot_df <- titles %>%
  inner_join(stats) %>%
  inner_join(video_word_count) %>%
  dplyr::select(c('X1','id','viewCount','likeCount','n','title_chr_count','dislikeCount'))

plot_df$n_scaled <- rescale(plot_df$n , to=c(0,1))

theme_plots <- theme(
  plot.title = element_text(hjust = 0.5,size=15)
)

plot_1 <- ggplot(plot_df)+
  geom_smooth(mapping = aes(x=n,title_chr_count)) +
  xlab('Word Count') +
  ylab('CharacterCount')+
  ggtitle('WordCount vs CharCount of Title') + 
  theme_bw()

plot_1+theme_plots

plot_2 <- ggplot(plot_df)+
  geom_smooth(mapping = aes(x=n,viewCount))+
  xlab('WordCount Of Title') +
  ylab('Views')+
  ggtitle('Views vs WordCount of Video') + 
  theme_bw()

plot_2+theme_plots

#############

data_modified <- plot_df %>%
  dplyr::select(c('X1','n_scaled')) %>%
  inner_join(data)

train_data_m <- data_modified[train_subset_ids,2:197]
test_data_m <- data_modified[-train_subset_ids,2:197]

train_h2o_m <- as.h2o(train_data_m, destination_frame = 'train_m.hex')
test_h2o_m <- as.h2o(test_data_m, destination_frame = 'test_m.hex')

splits_m <- h2o.splitFrame(
  data = train_h2o_m, ratios = 0.7, 
  seed = 1234)

train_data_h20_m <- splits_m[[1]]
valid_data_h20_m <-splits_m[[2]]

y_m <- 'viewCount'
x_m <- setdiff(names(train_data_h20_m),y)

model_4_m <- h2o.gbm(
  x=x_m,y=y_m,
  training_frame = train_data_h20_m,
  validation_frame = valid_data_h20_m,
  distribution = 'poisson',
  max_depth = 8,
  ntrees =58
)

plot(model_4_m)

predict_train_m <- h2o.predict(model_4_m,train_h2o_m)
prediction_train_m <- as.vector(predict_train_m)
actual_train_m <- train_data_m$viewCount
cor(prediction_train_m,actual_train_m)^2


predict_test_m <- h2o.predict(model_4_m,test_h2o_m)
prediction_test_m <- as.vector(predict_test_m)
actual_test_m <- as.vector(test_data_m$viewCount)
cor(prediction_test_m, actual_test_m)^2


h2o.shutdown(prompt = TRUE)