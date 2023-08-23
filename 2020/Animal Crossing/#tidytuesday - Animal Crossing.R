#===================================================================================================================
# Author: Aloysius Ong
# Dataset: #tidytuesday - Animal Crossing
# Purpose: Visualization 
#===================================================================================================================

setwd("C:/Users/User/Desktop")

#---- Libraries ----
library(dplyr)
library(ggcharts)
library(hrbrthemes)
library(viridis)
library(extrafont)
library(forcats)
library(gghighlight)
library(dplyr)
library(tibble)
library(ggThemeAssist)
library(data.table)
library(tidyverse)
library(rvest)
library(ggridges)
library(lubridate)
library(scales)
library(ggridges)
library(tidytext)
library(tidyverse)
library(dplyr)
library(debugr)
library(textdata)
library(stringr)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)

#---- Loading data ----
critic <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/critic.tsv')
user_reviews <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/user_reviews.tsv')
items <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/items.csv')
villagers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/villagers.csv')

#---- Exploring data ----

glimpse(critic)
glimpse(user_reviews)
glimpse(items)
glimpse(villagers)

#---- Looking at Items ----

summary(items)
table(items$category)

summary(critic)
summary(user_reviews)

items %>%
  group_by(name) %>%
  filter(sell_value > buy_value) %>%
  arrange(desc(sell_value))

items <- items %>%
  group_by(name) %>%
  mutate(profit = sell_value - buy_value)

summary(items)

items %>%
  arrange(desc(profit))

myvars <- c("category", "profit")
items_diverging <- items[myvars]

summary(items_diverging)

items_diverging <- items_diverging %>%
  group_by(category) %>%
  drop_na("profit") %>%
  mutate(mean_profit = mean(profit)) %>%
  ungroup()
items_diverging$profit <- NULL

items_diverging <- items_diverging[!duplicated(items_diverging), ]

items_diverging

bar_chart(items_diverging, category, mean_profit, 
          highlight = c("Flowers", "Fruit", "Bugs", "Fossils"), 
          sort = TRUE, limit = 20) + 
  theme_ft_rc() +
  theme(legend.position="none") +
  labs(title="Which Category of items in Animal Crossing are the most profitable?",
       subtitle = "Clearly, most of the items are not profitable in game, which sparked the creation of marketplaces like Nookazon. It serves\nthe role of a black market where there are no limits to prices. In addition, the money making scheme that has been spreading\nis the Stalk Market. Instead of common items, the goods that have been traded include rare villagers and turnips. Learning how\nto predict the Stalk Market has led to the rise of applications seeking to reverse engineer the market such as Turnip Prophet.\nThe data provided by VillageDB is unable to capture the ever-fluctuating prices of turnips. In addition, there is no price on the\nvillages and each player's take on the price of villagers is subjective. Further research needs to be done on the Nookazon to\ndiscover more insights on character-trafficking.",
       caption="Source: Forbes, 2020.\nVillageDB, 2020.\nDigital Trends, 2020.", 
       x = "",
       y = "Mean Profit") 

#---- Looking at Villagers ----

summary(critic)
critic$date <- as.Date(critic$date, "%Y-%m-%d")
class(critic$grade)

summary(user_reviews)
user_reviews$date <- as.Date(user_reviews$date, "%Y-%m-%d")
class(user_reviews$grade)

user_reviews1 <- user_reviews %>%
  group_by(date) %>%
  mutate(mean_grade_users = mean(grade))

varstodeluser <- c("grade", "user_name", "text")
user_reviews1[varstodeluser] <- NULL

critic1 <- critic %>%
  group_by(date) %>%
  mutate(mean_grade_critics = mean(grade)/10)

varstodel <- c("grade", "publication", "text")
critic1[varstodel] <- NULL

totalratings <- merge(critic1, user_reviews1, by=c("date"),all.x = TRUE, all.y = TRUE)

totalratings <- totalratings[!duplicated(totalratings), ]

totalratings

totalratingsMelted <- reshape2::melt(totalratings, id.var="date")

ggplot(totalratings, aes(x=date)) + 
  geom_point(aes(y = mean_grade_critics), color = "darkred") + 
  geom_point(aes(y = mean_grade_users), color="steelblue") +
  scale_fill_viridis(discrete=TRUE, guide=FALSE, option="A") +
  theme_ipsum() +
  theme(legend.position="bottom") +
  labs(title="How do Users and Critics rate Animal Crossing - New Horizons?",
       subtitle="",
       caption="Source: Metacritic, 2020", 
       x = "",
       y = "Rating")


ggplot(totalratingsMelted, aes(x=date, y=value, col=variable)) + geom_point() +
  scale_fill_viridis(discrete=TRUE, guide=FALSE, option="C") +
  theme_ipsum() +
  theme(legend.position="bottom") +
  labs(title="How do Users and Critics rate Animal Crossing - New Horizons?",
       subtitle="Critics have been praising the game for its anime like gameplay, and calls it the best distraction from the real world, especially during Covid-19.\nHowever, users are not pleased with the game, mainly because of how Nintendo only allowed one island per console. This has made many fans\nunhappy as some of them are playing the game with their partners and believe that it is not justifiable to limit the gameplay experience to only one\nperson. Especially, when it was marketed as follows: Whether playing online or with others beside you, island living is even better when you can\nshare it. No wonder the game has received diverse ratings. Nevertheless, it is still said to be the best Animal Crossing game yet.",
       caption="Source: Metacritic, 2020", 
       x = "",
       y = "Rating")


#---- Wordcloud of User Reviews ----
user_reviewstext <- Corpus(VectorSource(user_reviews$text))


user_reviewstextClean<-tm_map(user_reviewstext, PlainTextDocument)
user_reviewstextClean<-tm_map(user_reviewstext,tolower)
user_reviewstextClean<-tm_map(user_reviewstextClean,removeNumbers)
user_reviewstextClean<-tm_map(user_reviewstextClean,removeWords,stopwords("english"))
user_reviewstextClean<-tm_map(user_reviewstextClean,removePunctuation)
user_reviewstextClean<-tm_map(user_reviewstextClean,stripWhitespace)
user_reviewstextClean<-tm_map(user_reviewstextClean,stemDocument)

wordcloud(words = user_reviewstextClean, min.freq = 2,
          max.words=100, random.order=FALSE, 
          #rot.per=0.35, 
          colors=brewer.pal(8, "Set2"))

wordcloud(user_reviewstextClean, scale=c(5,0.5), max.words=100, random.order=FALSE, rot.per=0.35, 
          use.r.layout=FALSE, colors=brewer.pal(8, "Set2"))


dtm <- TermDocumentMatrix(user_reviewstextClean)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Set2"))

#---- WordCloud of critics ----
  
critictext <- Corpus(VectorSource(critic$text))
  
  
critictextClean <- tm_map(critictext, PlainTextDocument)
critictextClean <- tm_map(critictext,tolower)
critictextClean <- tm_map(critictextClean,removeNumbers)
critictextClean <- tm_map(critictextClean,removeWords,stopwords("english"))
critictextClean <- tm_map(critictextClean,removePunctuation)
critictextClean <- tm_map(critictextClean,stripWhitespace)
critictextClean <- tm_map(critictextClean,stemDocument)
  
system.time(wordcloud(words = critictextClean, min.freq = 2,
          max.words=200, random.order=FALSE, 
          rot.per=0.35, 
          colors=brewer.pal(8, "Set2")))
  














