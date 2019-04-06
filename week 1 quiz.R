

#Question - 2
twitter <- readLines(con <- file("en_US.twitter.txt"), encoding = "UTF-8", skipNul = TRUE)
length(twitter)

#Question 3
#What is the length of the longest line seen in any of the three en_US data sets?

# Blogs file
blogs<-file("en_US.blogs.txt","r")
blogs_lines<-readLines(blogs)
close(blogs)
summary(nchar(blogs_lines))

#News file
news<-file("en_US.news.txt","r")
news_lines<-readLines(news)
close(news)
summary(nchar(news_lines))

#twitter
summary(nchar(twitter))

#Question 4
#In the en_US twitter data set, if you divide the number of lines where the word "love" (all lowercase) 
#occurs by the number of lines the word "hate" (all lowercase) occurs, about what do you get?

love<-length(grep("love", twitter))
hate<-length(grep("hate", twitter))
love/hate

#Question 5
#The one tweet in the en_US twitter data set that matches the word "biostats" says what?
grep("biostats", twitter, value = T)

#Question 6
#How many tweets have the exact characters "A computer once beat me at chess, but it was no match for me at kickboxing". 
#(I.e. the line matches those characters exactly.)

grep("A computer once beat me at chess, but it was no match for me at kickboxing", twitter)


