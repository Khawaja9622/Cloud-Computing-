rm(list=ls())

getwd()
setwd("/Users/khawajahassan/ceu-cloud-class/serverless")
library(rvest)
install.packages("aws.comprehend", repos = c(cloudyr = "http://cloudyr.github.io/drat", getOption("repos")))
library(xml2)
library(dplyr)
library(data.table)
library(httr)
library("aws.comprehend")
library(ggthemes)
library(ggplot2)
library(tidyr)
library(wordcloud)
library(tidyverse)

""


# *Installation*
#   Latest Stable Version:

# On Mac/Linux:
if (Sys.info()["sysname"] == 'Darwin'){
  Sys.setenv(LDFLAGS="-L/usr/local/opt/openssl@1.1/lib",
             CPPFLAGS="-I/usr/local/opt/openssl@1.1/include",
             PKG_CONFIG_PATH="/usr/local/opt/openssl@1.1/lib/pkgconfig",
             LIBRARY_PATH=paste(Sys.getenv("LIBRARY_PATH"),
                                "/usr/local/opt/openssl@1.1/lib",
                                sep=""))
  dir.create(path = Sys.getenv("R_LIBS_USER"), showWarnings = FALSE, recursive = TRUE)
  install.packages("xml2", configure.vars='INCLUDE_DIR=/usr/local/opt/libxml2/include/libxml2 LIB_DIR=/usr/local/opt/libxml2/lib/')
  install.packages('curl', lib = Sys.getenv("R_LIBS_USER"))
  install.packages('httr')
  install.packages("aws.s3", repos = c("cloudyr" = "http://cloudyr.github.io/drat"))
} else { # On Windows
  install.packages("aws.s3", repos = c("cloudyr" = "http://cloudyr.github.io/drat"), INSTALL_opts = "--no-multiarch")
  # if not working use:
  # install.packages("aws.s3", repos = c("cloudyr" = "http://cloudyr.github.io/drat"))
}

# Set up your R w/ AWS

keyfile = list.files(path=".", pattern="accessKeys.csv", full.names=TRUE)
if (identical(keyfile, character(0))){
  stop("ERROR: AWS key file not found")
} 
keyTable <- read.csv(keyfile, header = T)
AWS_ACCESS_KEY_ID <- as.character(keyTable$Access.key.ID)
AWS_SECRET_ACCESS_KEY <- as.character(keyTable$Secret.access.key)
#activate
Sys.setenv("AWS_ACCESS_KEY_ID" = AWS_ACCESS_KEY_ID,
           "AWS_SECRET_ACCESS_KEY" = AWS_SECRET_ACCESS_KEY,
           "AWS_DEFAULT_REGION" = "eu-west-1") 

library("aws.comprehend")

# TRIBUNE PAKISTAN
tp<- read_html("https://www.dawn.com/news/1466951")
description <- tp %>% html_nodes('p:nth-child(31) , p:nth-child(29) , p:nth-child(30) , p:nth-child(28) , p:nth-child(26) , p:nth-child(25) , p:nth-child(24) , p:nth-child(22) , #5c7a21518815b+ p , .mt-1 p:nth-child(1) , .border-b-solid .story__link') %>% html_text()
detect_language(description)
Tribune_Pak<- detect_sentiment(description)
Tribune_Pak

Tribune_Pak$Sentiment <- NULL
pakistan_news <- Tribune_Pak %>% gather("sentiment", "score", -1)
ggplot(data = pakistan_news ,aes(x= Index, y = score ,fill = sentiment))+
  geom_bar(stat="identity", position ="fill")+
  ggtitle("Pakistan Tribune")+
  theme_wsj()



# TRIBUNE INDIA
ti<- read_html("https://www.tribuneindia.com/news/nation/pak-army-chiefs-legs-were-shaking-as-qureshi-said-india-would-attack-if-abhinandan-not-freed-162794")
description1 <- ti %>% html_nodes('.story-desc p , h1') %>% html_text()
detect_language(description1)
Tribune_In<- detect_sentiment(description1)
Tribune_In$Sentiment <-NULL 
# Long to Wide Format
Indian_news <- Tribune_In %>% gather("sentiment", "score", -1)


# Graphics indian_news
 ggplot(data = indian_news,aes(x= Index, y = score ,fill = sentiment))+
 geom_bar(stat="identity", position ="fill")+
   ggtitle("Hindustan Tribune")+
   theme_wsj()



# prime minister statement 

# Indian perspective
modi<- read_html("https://scroll.in/latest/920832/qatal-ki-raat-narendra-modi-says-his-warning-forced-islamabad-to-send-back-captured-iaf-pilot")
description2 <- modi %>% html_nodes('#article-contents p:nth-child(2)') %>% html_text()
detect_language(description2)
modi_stat<- detect_sentiment(description2)
modi_stat$Sentiment <- NULL
modi_stat

 a <- modi_stat %>% gather("sentiment", "score", -1)
 is.data.frame(a)
 as.factor(a$sentiment)
 a$fraction <- a$score / sum(a$score)
 a$ymax <- cumsum(a$fraction)
 a$ymin <- c(0, head(a$ymax, n=-1))
 
 
 
 pie1 <- ggplot(a, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=sentiment)) +
   geom_rect() +
   geom_text( x=-1, aes(y=0, label="NARENDRA-MODI"), size=6) + # x here controls label position (inner / outer)
   scale_fill_brewer(palette=3) +
   scale_color_brewer(palette=3) +
   coord_polar(theta="y") +
   xlim(c(-1, 4)) +
   theme_void() 
 pie1


# Pakistan perspective
imran<- read_html("https://www.dawn.com/news/1466723")
description3 <- imran %>% html_nodes('.story__content--normal p:nth-child(2)') %>% html_text()
detect_language(description3)
imran_stat<- detect_sentiment(description3)
imran_stat
imran_stat$Sentiment <- NULL

b <- imran_stat %>% gather("sentiment", "score", -1)
is.data.frame(b)
as.factor(a$sentiment)
b$fraction <- b$score / sum(b$score)
b$ymax <- cumsum(b$fraction)
b$ymin <- c(0, head(b$ymax, n=-1))


pie <- ggplot(b, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=sentiment)) +
  geom_rect() +
  geom_text( x=-1, aes(y=0, label="IMRAN-KHAN"), size=6) + # x here controls label position (inner / outer)
  scale_fill_brewer(palette=3) +
  scale_color_brewer(palette=3) +
  coord_polar(theta="y") +
  xlim(c(-1, 4)) +
  theme_void() 
pie




 install.packages("wordcloud")
 library(wordcloud)
 install.packages("RColorBrewer")
 library(RColorBrewer)
 library(tm)
 

# word_cloud_India
 
 docs <- Corpus(VectorSource(paste(description1, collapse = '')))
 inspect(docs)
 dtm <- TermDocumentMatrix(docs)
 m <- as.matrix(dtm)
 v <- sort(rowSums(m),decreasing=TRUE)
 d <- data.frame(word = names(v),freq=v)
 d <- d[-c(1,2,3,4,5),]
 d
 head(d, 10)
 set.seed(1234)
 wordcloud(words = d$word, freq = d$freq, min.freq = 1,
           max.words=180, random.order=TRUE, rot.per=0.35, 
           colors=brewer.pal(8, "Dark2"))
 
 # word-cloud1 Pak
 
 docs1 <- Corpus(VectorSource(paste(description, collapse = '')))
 inspect(docs1)
 dtm1 <- TermDocumentMatrix(docs1)
 m <- as.matrix(dtm1)
 v1 <- sort(rowSums(m),decreasing=TRUE)
 d1 <- data.frame(word = names(v1),freq=v1)
 head(d1, 10)
 d1 <- d1[-c(2,3),]
 set.seed(1234)
 wordcloud(words = d1$word,freq = d1$freq, min.freq = 1,
           max.words=100, random.order=TRUE, rot.per=0.35, 
           colors=brewer.pal(8, "Dark2"))
  
 

