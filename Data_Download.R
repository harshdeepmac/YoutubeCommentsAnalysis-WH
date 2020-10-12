rm(list = ls())
library(tidyverse)
library("tuber")
#install.packages("tuber")
library(devtools)

api_key<- "***********"
client_id <- "*********************"
client_secret <- "********************"

white_house_channek_id <- 'UCYxRlFDqcWM4y7FfpiAN3KQ'
yt_oauth(app_id = client_id, app_secret = client_secret, token = "")

white_house_videos <- list_channel_videos(channel_id = white_house_channek_id, max_results = 200)


videos_2020 <- white_house_videos %>%
  filter(as.Date(contentDetails.videoPublishedAt)  >= as.Date('2020-01-01')) %>%
  select(contentDetails.videoId)
videos_2020<- as.vector(videos_2020$contentDetails.videoId)

video_id <- as.vector(unique(df$videoId))

comments_for <-data.frame()
for(i in videos_2020){
  comments_for <- rbind(comments_for, get_comment_threads(filter = c(video_id = i), max_results = Inf))
}

#write.csv(comments_for, file ='Comments_2020.csv')

a<-list()
cont<-1
for (i in white_house_videos$contentDetails.videoId){
  b<-list(get_video_details(video_id = i))
  a<- append(a,b)
  print(cont)
  cont <- cont+1
}

titles<- c()
publishedDates <-c()
video_ids <-c()

for (i in a){
  titles<-c(titles,(i$items[[1]]$snippet$title))
  publishedDates<-c(publishedDates,(i$items[[1]]$snippet$publishedAt))
  video_ids<- c(video_ids,(i$items[[1]]$id))
}
titles_df <-data.frame(Video_ID= video_ids, Title=titles, Upload_Date = publishedDates)


video_stats <- map_df(titles_df$Video_ID, ~get_stats(.))

overall_stats <- video_stats %>%
  left_join(titles_df, by=c('id'='Video_ID'))

#write.csv(titles_df,'Titles.csv')
#write.csv(video_stats,'All_Stats.csv')

