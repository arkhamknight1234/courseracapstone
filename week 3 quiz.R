library(stringr)
library(stringi)
library(tm)
library(wordcloud)
library(ggplot2)

setwd("C:/Users/Sheetal/datasciencecoursera/capstoneproject/Coursera-SwiftKey/final/en_US")

#Select ALL DOCUMENTS in Blogs
allBlogs <- readLines("en_US.blogs.txt")
#Select ALL DOCUMENTS in News
allNews <- readLines("en_US.news.txt")
#Select ALL DOCUMENTS in Twitters
allTwitter <- readLines("en_US.twitter.txt")

summary(allBlogs)

tokenmaker <- function(x) {
  corpus <- Corpus(VectorSource(x))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, PlainTextDocument)
  #        corpus <- tm_map(corpus, stemDocument)
  corpus <- Corpus(VectorSource(corpus))
}  

wordcounter <- function(x) {
  dtm<-DocumentTermMatrix(x)
  dtm_matrix <- as.matrix(dtm)
  word_freq <- colSums(dtm_matrix)
  word_freq <- sort(word_freq, decreasing = TRUE)
  words <- names(word_freq)
  return(list(words, word_freq))
}  

NextWordIs <- function(x,y){
  BQuest<-grepl(x, allBlogs, ignore.case=TRUE)
  BDocs<-allBlogs[BQuest]
  textoachado<-'a'
  NextWordIs<-'a'
  i<-length(BDocs)
  if (i>0)
  {
    for (i in 1:i)
    {  textoachado[i]<- str_extract(BDocs[i], y)
    NextWordIs[i]<- stri_extract_last_words(textoachado[i]) 
    }
  }
  NQuest<-grepl(x, allNews, ignore.case=TRUE)
  NDocs<-allNews[NQuest]
  j=length(NDocs)
  if (j>0)
  {
    for (j in 1:j)
    {  textoachado[i+j]<- str_extract(NDocs[j], y)
    NextWordIs[i+j]<- stri_extract_last_words(textoachado[i+j]) 
    }
  }
  TQuest<-grepl(x, allTwitter, ignore.case=TRUE)
  TDocs<-allTwitter[TQuest]
  k=length(TDocs)
  if (k>0)
  {
    for (k in 1:k)
    {  textoachado[i+j+k]<- str_extract(TDocs[k], y)
    NextWordIs[i+j+k]<- stri_extract_last_words(textoachado[i+j+k]) 
    }
  }
  bundle<-as.data.frame(NextWordIs, stringsAsFactors=FALSE)
  summary (bundle)
  blogs_token <- tokenmaker(bundle)
  blogs_words <- wordcounter(blogs_token)
  summary(nchar(bundle))
  head(bundle)
  tdm_Blogs<-TermDocumentMatrix(blogs_token)
  m_Blogs<-as.matrix(tdm_Blogs)
  v_Blogs<-sort(rowSums(m_Blogs),decreasing=TRUE)
  d_Blogs<-data.frame(word=names(v_Blogs),freq=v_Blogs)
  head(v_Blogs, 100)    
  return(list(head(v_Blogs,100)))
}
#Question 1
resultado_01<-NextWordIs("a case of ", "([Aa]+ +[Cc]ase+ +[Oo]f+ +[^ ]+ )" )
resultado_01

#Question 2
resultado_02<-NextWordIs("would mean the ", "([Ww]ould+ +[Mm]ean+ +[Tt]he+ +[^ ]+ )" )  
resultado_02

#Question 3
resultado_03<-NextWordIs("make me the ", "([Mm]ake+ +[Mm]e+ +[Tt]he+ +[^ ]+ )" )  
resultado_03

#Question 4 - didnt work
resultado_04<-NextWordIs("struggling ", "([Ss]truggling+ +[^ ]+ +[^ ]+ +[^ ]+ )" )  
resultado_04

#Question 5 - didnt work
resultado_05<-NextWordIs("date at the ", "([Dd]ate+ +[Aa]t+ +[Tt]he+ +[^ ]+ )" )  
resultado_05

#Question 6
resultado_06<-NextWordIs("be on my ", "([Bb]e+ +[Oo]n+ +[Mm]y+ +[^ ]+ )" )  
resultado_06

#Question 7
resultado_07<-NextWordIs("quite some ", "([Qq]uite+ +[Ss]ome+ +[^ ]+ )" )  
resultado_07  

#Question 8
resultado_08<-NextWordIs("his little ", "([Hh]is+ +[Ll]ittle+ +[^ ]+ )" )  
resultado_08  

#Question 9 - didnt work
resultado_09<-NextWordIs("during the ", "([Dd]uring+ +[TT]he+ +[^ ]+ )" )  
resultado_09

#Question 10
resultado_10<-NextWordIs("must be ",  "([Mm]ust+ +[Bb]e+ +[^ ]+ )" )  
resultado_10  

