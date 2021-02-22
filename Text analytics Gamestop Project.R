#########################################
#Business Insight Report
#by Sarah Medina
#########################################

#loading packages
#calling libararies
library(rvest)
library(tidyverse)
library(tidytext)
library(dplyr)
library(tidyr)
library(scales)
library(ggplot2)
library(textdata)
library(stringr)
library(wordcloud)
library(reshape2)
library(forcats)
library(tm)
library(igraph)
library(ggraph)

#yahoofinance
yahoo <- read_html("https://finance.yahoo.com/news/gamestop-amc-reddit-investing-213609595.html")

#getting article title
yahootitle <- yahoo%>%
  html_nodes("title")%>%
  html_text()

#viewing article title
yahootitle

#getting text from the article webpage
yahootxt <- yahoo%>%
  html_nodes("p")%>%
  html_text()
#yahootxt

#TIME
timemag <- read_html("https://time.com/5933242/gamestop-stock-gme/")

#getting article title
timemagtitle <- timemag%>%
  html_nodes("title")%>%
  html_text()

#viewing article title
timemagtitle

#getting text from the article webpage
timemagtxt <- timemag%>%
  html_nodes("p")%>%
  html_text()
#timemagtxt

#Nytimes
nytimes <- read_html("https://www.nytimes.com/2021/02/01/business/gamestop-how-much-worth.html")

#getting article title
nytitle <- nytimes%>%
  html_nodes("title")%>%
  html_text()

#viewing article title
nytitle

#getting text from the article webpage
nytxt <- nytimes%>%
  html_nodes("p")%>%
  html_text()
#nytxt

#creating a vector for each article 
#yahoo data frame
yahoodf<- data.frame(line=1, text= yahootxt, stringsAsFactors = FALSE)
#calling the yahoo data frame
#yahoodf

#ny time dataframe
ny_df<- data.frame(line=1, text= nytxt, stringsAsFactors = FALSE)

#time magazine dataframe
timemag_df<- data.frame(line=1, text= timemagtxt, stringsAsFactors = FALSE)

#################
#tokenizing text
################
#tokenizing yahoo
yahoo_token<- yahoodf%>%
  unnest_tokens(word, text)%>%
  anti_join(stop_words)%>%
  count(word, sort = TRUE)
#calling the yahoo token
#yahoo_token

#tokenizing the time magazine data frame
timemag_token<- timemag_df%>%
  unnest_tokens(word, text)%>%
  anti_join(stop_words)%>%
  count(word, sort = TRUE)
#calling the time token
#timemag_token

#tokenizing the ny times dataframe
ny_token<- ny_df%>%
  unnest_tokens(word, text)%>%
  anti_join(stop_words)%>%
  count(word, sort = TRUE)
#calling ny token
#ny_token

################
#histogram frequencies
############
#looking at the top 8 to 10 frequencys in words tokens
#plotting token fequencies through histograms
#yahoo histogram
yahoo_hist <- yahoodf%>%
  unnest_tokens(word, text)%>%
  anti_join(stop_words)%>%
  count(word, sort=TRUE) %>%
  filter(n > 13) %>% # we need this to eliminate all the low count words
  mutate(word = reorder(word,n )) %>%
  ggplot(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()

#printing  frequency token histogram for yahoo
print(yahoo_hist + ggtitle("Yahoo Histogram"))

#plotting token fequencies through histograms
#tinme mag histogram
timemag_hist <- timemag_df%>%
  unnest_tokens(word, text)%>%
  anti_join(stop_words)%>%
  count(word, sort=TRUE) %>%
  filter(n > 3) %>% # we need this to eliminate all the low count words
  mutate(word = reorder(word,n )) %>%
  ggplot(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()

#printing  frequency token histogram for time mag
print(timemag_hist+ggtitle("Time Mag Histogram"))

#plotting token fequencies through histograms
#ny times histogram
ny_hist <- ny_df%>%
  unnest_tokens(word, text)%>%
  anti_join(stop_words)%>%
  count(word, sort=TRUE) %>%
  filter(n > 6) %>% # we need this to eliminate all the low count words
  mutate(word = reorder(word,n )) %>%
  ggplot(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()

#printing  frequency token histogram for ny times
print(ny_hist+ggtitle("NY Times Histogram"))

#removing stop words but no using count like in token_ 
tidy_yahoo<- yahoodf%>%
  unnest_tokens(word, text)%>%
  anti_join(stop_words)

tidy_timemag<- timemag_df%>%
  unnest_tokens(word, text)%>%
  anti_join(stop_words)

tidy_ny<- ny_df%>%
  unnest_tokens(word, text)%>%
  anti_join(stop_words)

###########
#frequcnies
#########

#combining all three datasets and looking at the articles frequencies
#comparing frequnecies to yahoo
frequency <- bind_rows(mutate(tidy_yahoo, author = "Yahoo"),
                         mutate(tidy_timemag, author = "TIME"),
                         mutate(tidy_ny, author = "NYtimes")) %>%
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n/sum(n))%>%
  select(-n) %>%
  spread(author, proportion) %>%
  gather(author, proportion, `TIME`, `NYtimes`)

#printing the frequency
print(frequency)

#plotting correlograms to look at articles compared to yahoo
ggplot(frequency, aes(x=proportion, y=`Yahoo`, 
                        color = abs(`Yahoo`- proportion)))+
  geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~author, ncol=2)+
  theme(legend.position = "none")+
  labs(y= "Yahoo", x=NULL)

#looking at correlations
#corr of time and yahoo
cor.test(data=frequency[frequency$author == "TIME",],
         ~proportion + `Yahoo`)
#corr of ny times and yahoo
cor.test(data=frequency[frequency$author == "NYtimes",],
         ~proportion + `Yahoo`)  


#############
#sentiment analysis
############
#calling the sentiments
afinn<- get_sentiments("afinn")
nrc<- get_sentiments("nrc")
bing<- get_sentiments("bing")

#yahoo sentiment
tidy_yahoo%>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

tidy_yahoo %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T)

yahoosentiment <-tidy_yahoo %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

#plotting all of the sentiments for yahoo
yahoosentiment %>%
  group_by(sentiment) %>%
  top_n(8) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to Sentiment Yahoo", x=NULL)+
  coord_flip()

#filtering for positive sentiment in yahoo
yahoosentiment %>%
  group_by(sentiment) %>%
  filter(sentiment == "positive")%>%
  top_n(8) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +ggtitle("Yahoo Positive")+
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Anticipation Sentiment- Yahoo", x=NULL)+
  coord_flip()

#filtering for negative sentiment in yahoo
yahoosentiment %>%
  group_by(sentiment) %>%
  filter(sentiment == "negative")%>%
  top_n(8) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) + ggtitle("Yahoo Negative")+
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Negative Sentiment- Yahoo", x=NULL)+
  coord_flip()

#TIME mag  sentiment
tidy_timemag%>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

tidy_timemag %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T)

timemagsentiment <-tidy_timemag %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

#plotting all of the sentiments in ggplot for time mag
timemagsentiment %>%
  group_by(sentiment) %>%
  top_n(7) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to Sentiment TIME", x=NULL)+
  coord_flip()

#filtering for positive sentiment in timemag
timemagsentiment %>%
  group_by(sentiment) %>%
  filter(sentiment == "positive")%>%
  top_n(8) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +ggtitle("Time Positive")+
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Anticipation Sentiment- TIME", x=NULL)+
  coord_flip()

#filtering for negative sent9iment in time mag
timemagsentiment %>%
  group_by(sentiment) %>%
  filter(sentiment == "negative")%>%
  top_n(8) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +ggtitle("Time Negative")+
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Negative Sentiment- TIME", x=NULL)+
  coord_flip()

#NY times sentiment 
tidy_ny%>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

tidy_ny %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T)

nysentiment <-tidy_ny %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

#looking at all sentiments for ny times
nysentiment %>%
  group_by(sentiment) %>%
  top_n(8) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to Sentiment NYtimes", x=NULL)+
  coord_flip()

#filtering for positive sentiment in NY times
nysentiment %>%
  group_by(sentiment) %>%
  filter(sentiment == "positive")%>%
  top_n(8) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +ggtitle("NY Times Positive")+
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Anticipation Sentiment- NYtimes", x=NULL)+
  coord_flip()

#filtering for negative sentiment in NY times
nysentiment %>%
  group_by(sentiment) %>%
  filter(sentiment == "negative")%>%
  top_n(8) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +ggtitle("NY Times Negative")+
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Negative Sentiment- NYtimes", x=NULL)+
  coord_flip()

######################
#TF IDF
#####################
#combining the articles
#taking out benzinga
cust_stop<- data_frame(word= c("benzinga.com", "benzinga", "ev", "cannabis", "ocugen"),
                    lexicon = rep("cust", each =5))

#combining all the articles
combined_articles<- bind_rows(mutate(yahoodf, author = "Yahoo"),
                              mutate(timemag_df, author = "TIME"),
                              mutate(ny_df, author = "NYtimes"))


combined_articles1<- combined_articles%>%
  unnest_tokens(word, text)%>%
  anti_join(stop_words)%>%
  anti_join(cust_stop)%>%
  count(author, word, sort = TRUE)%>%
  ungroup()

total_words <- combined_articles1%>%
  group_by(author) %>%
  summarize(total=sum(n))

combined_articles1<- left_join(combined_articles1, total_words)

tidy_arts <- combined_articles1 %>%
  bind_tf_idf(word, author, n)

#Graphing single words tfidfs
# looking at the graphical apporach:
tidy_arts %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(author) %>%
  top_n(10) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = author)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~author, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)

##################
#Word Clouds
################
combined_articles2<- combined_articles%>%
  unnest_tokens(word, text)%>%
  anti_join(stop_words)%>%
  anti_join(cust_stop)%>%
  count(author, word, sort = TRUE)
#word cloud for all three articles
#nrc word cloud

combined_articles2 %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 300)

#bing word cloud
#all sentiments 
combined_articles2 %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 200)

##############
#bigrams
##############
#combining the articles
combined_articles<- bind_rows(mutate(yahoodf, author = "Yahoo"),
                              mutate(timemag_df, author = "TIME"),
                              mutate(ny_df, author = "NYtimes"))

#calling the article bigram
Article_bigram <- combined_articles %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2) %>%
  filter(!is.na(bigram))%>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  count(word1, word2, sort = TRUE)
Article_bigram

#######
a_bigrams <- combined_articles%>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)
a_bigrams
########

bigram <- combined_articles %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word1 %in% cust_stop$word) %>%
  filter(!word2 %in% cust_stop$word) 

#creating a bigram graph
bigram_graph <- Article_bigram %>%
  filter(n>2) %>% #use a lower n in project
  graph_from_data_frame()

#calling bigram graph
bigram_graph

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)


#new terms bigrams united
bigrams_united <- bigram %>%
  unite(bigram, word1, word2, sep = " ")



#################
#bigrams tfidf
##############
#new tf idf term
bigram_tf_idf <- bigrams_united %>%
  count(author, bigram) %>%
  bind_tf_idf(bigram, author, n) %>%
  arrange(desc(tf_idf))

#using tfidf to look at multiple words and their scores
bigram_tf_idf %>%
  group_by(author) %>%
  slice_max(tf_idf, n = 4) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(bigram, tf_idf), fill = author)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~author, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)
  coord_flip()



#creating a histogram for all
all_articles<- bind_rows(mutate(yahoodf, author = "Yahoo"),
                         mutate(timemag_df, author = "TIME"),
                        mutate(ny_df, author = "NYtimes"))
#creating a histiogram for all of teh articles
hist<- all_articles%>%
  unnest_tokens(word, text)%>%
  anti_join(stop_words)%>%
  count(word, sort=TRUE) %>%
  filter(n > 15) %>% # we need this to eliminate all the low count words
  mutate(word = reorder(word,n )) %>%
  ggplot(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()
print(hist+ggtitle("All Articles Histogram"))

#calling the library
library(topicmodels)
#document term matrix for all articles
#looking at sparsity percentage
a_dtm <- combined_articles %>%
  unnest_tokens(word, text) %>%
  count(author, word) %>%
  cast_dtm(author, word, n)
#printing
a_dtm

articles_dtm<- combined_articles1%>%
  cast_dtm(author, word, n)

#LDA
articles_lda<- LDA(articles_dtm, k=3, control=list(seed=123))
articles_lda

#new df named ap_topics 
ap_topics <- tidy(articles_lda, matrix = "beta")
#printing ap topics
ap_topics

ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

#BETA ANanysis
#calling ggplot beta for all 3 articles and comparisons 
ap_top_terms %>%
  mutate(term = reorder(term, beta)) %>% 
  ggplot(aes(term, beta, fill = factor(topic))) + geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") + coord_flip()

#creating a new term beta spread
beta_spread <- ap_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

#calling the beta spread
beta_spread




