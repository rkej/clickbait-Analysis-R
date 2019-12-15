---
title: "Clickbait-analysis"
output: html_document
---
# Research Question
How to best tell if a video is click-bait or not?

## Overview
We chose this topic because we like watching YouTube videos. A big issue with finding a Youtube you enjoy is avoiding clickbait videos. When you click on one, you waste time realizing its a clickbait video and then have to search for what you want again. There are many features of a video you can look at to tell that it is a clickbait video without having to watch part of the video. In this analysis, we will identify these features.  

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
```{r}
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyverse)
library(stringi)
```


```{r}
library(qdap)
library(SentimentAnalysis)
```
### Imports and starting analysis

Here we load and get our data ready for analysis. The first dataset is from scraping YouTube videos and taking all features off it. We took title, likes, dislikes, views, description, comments, comment count, and if its a clickbait video or not. The second data set is a kaggle data set with many more titles so we can analyize the specific titles of videos better. The reasoning behind wanting more titles is because that is the easiest feature to look at to tell if a video is clickbait so we wanted to look at that further. 
```{r}
ClickbaitData <- read.csv("dataset.csv") #import 1
ClickbaitData2 <- read.csv("clickBait_Data.csv") #import 2
ClickbaitData2 <- ClickbaitData2 %>%
  mutate(clickbait = recode(clickbait, `0` = "False", `1` = "True")) %>%
  rename(class = clickbait) %>%
  rename(title = titles)
JoinedData <- merge(x = ClickbaitData, y = ClickbaitData2, by = "title", all = TRUE)
JoinedData %>%
  head()
```
### Analysis 2: Trying spreading the data by classes. 

```{r}

SpreadTest <- ClickbaitData %>% 
  spread(key=class, value=dislikeCount)
head(SpreadTest)

# unstack narrow to wide
```
We start to see that dislikeCount has some kind of impact when we observed that there are higher number of dislikes on clickbait videos. 

### Analysis 3: Exploring the dataset by filtering the classes
Here we are analyzing the data more and looking into what videos are clickbait and not. 
```{r}
# analyzing data
IsClickbait <- ClickbaitData %>%
  filter(class != "False")
NotClickbait <- ClickbaitData %>%
  filter(class == "False")
head(NotClickbait)
```

### Analysis 4: Analysing how length of titles are related to class
Looking at the plot below, we can see that there is a normal range of title lengths from 10-100 characters and then if a video is clickbait, it can be less that the 10 character average. 

```{r}
 
LenData <- ClickbaitData %>%
  mutate(titleLen = stri_length(title))
LenData <- ClickbaitData %>%
  mutate(titleLen = stri_length(title))
LenData %>%
  ggplot(aes(x = class, y = titleLen)) + geom_point()


```

### Analysis 5: Analysing dislikeCount as a factor
Here we see if a video has more dislikes, it is going to have a much higher chance to be a clickbait video. 
```{r}
LenData %>%
  head(2000) %>%
  group_by(class) %>%
  summarise(dislikeCount = mean(dislikeCount))%>%
  ggplot(aes(x = class, y = dislikeCount)) + geom_bar(stat = "identity")
```
The above graph made a lot of sense. We do not like clickbait videos so many people would dislike it. Now on clickbate videos, uploaders can give it fake likes so next we wanted to see if there was a relationship to the ratio of likes to dislikes. We see that no matter the likes, there will still be a high amount of dislikes telling us that a video is clickbait. 
### Analysis 6: Analysing the ratio of dislikes to likes as a factor
Now, we check for the LikeCounts vs DislikeCounts and analyze if their ratio is making a difference. 
```{r}
LenData %>%
  head(1000) %>%
  ggplot(aes(x = likeCount, y = dislikeCount, color = class)) + geom_point()
```
It looks like a video being a clickbait has a higher dislike to likes ratio and also has many outliers. 
### Analysis 7: Sentimental Analysis
Up next for analysis is sentimental analysis. Using the Qdap's library function to get polarities of a string, we see if clickbait titles have more polarities - meaning being more positive, happy, which would attract people to click on these videos. 
```{r}
PolData<- LenData %>%
  mutate(polarity_title = counts(polarity(title))[, "polarity"])
PolData%>%
  group_by(class)%>%
  summarise(meanPol = mean(polarity_title))%>%
  ggplot(aes(x = class, y = meanPol)) + geom_bar(stat = "identity")

```
We see there's a huge difference in polarities and a clickbait video is more likely to have higher levels of polarities. 
### Decision Tree
Since we found that polarities and the ratio of likes to dislikes are the strongest factors in deciding whether a video is clickbait or not, we decided to see what a decision tree would look like considering both of these factors. 
```{r}
#Decision Tree
PolData<- PolData%>%
  mutate(class = recode(class, `True` = 1, `False` = 0))

mod1 <- party::ctree(
  class ~ polarity_title + likeCount/dislikeCount,
  data=PolData) 
plot(mod1, type="simple")
```
