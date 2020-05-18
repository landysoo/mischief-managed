library(dplyr)
library(stringr)
library(tidytext)
library(textreadr)
library(ggplot2)
library(reshape2)
library(stopwords)
library(tidyr)
library(tidyverse)

#Importing txt files ( The Hobbit - Book and Trilogy Script)
hobbit_book <- read_document(file='C:/Users/carla/Desktop/HULT/MSBA 2019-20/Courses/Data Science R/Text Analytics/the hobbit.txt')
hobbit_Script <- read_document(file='C:/Users/carla/Desktop/HULT/MSBA 2019-20/Courses/Data Science R/Text Analytics/Hobbit Script Trilogy.txt')

#Concatenating vectors of txt files. 
book_h <- paste(hobbit_book, collapse = " ") 
script_h <- paste(hobbit_Script, collapse = " ") 


#Putting vectors in data frame format 
book_df <- data_frame(line=1:5, text=book_h)
script_df <- data_frame(line=1:5, text=script_h)


###########################################################
#Tokenizing data frames 
###########################################################

#Customise stop words 
stop_words_hobbit <- data_frame(word = c('â', 'oi', 's', 'about', 'a', 'carefully', 'it', 'o_', 'oit', '_â', 'youâ', '"',
                                         'll', 'itâ'), lexicon = rep('CUST', each =14))


#Hobbit Book Tokenization
book_tokens <- book_df %>%
  unnest_tokens(word, text)%>% 
  anti_join(stop_words) %>% 
  anti_join(stop_words_hobbit) %>% 
  count(word, sort=TRUE)

#Hobbit Script Tokenization
script_tokens <- script_df %>%
  unnest_tokens(word, text)%>% 
  anti_join(stop_words) %>% 
  anti_join(stop_words_hobbit) %>% 
  count(word, sort=TRUE)


##View(hobbit_st)

###########################################################
#Binding data frames together to calculate frequency of all. 
###########################################################


frequency <- bind_rows(mutate(book_tokens, version = "Hobbit Book"),
                       mutate(script_tokens, version = "Hobbit Script")) %>%
  mutate(word = str_extract(word, "[a-z']+")) %>% # Use str_extract because UTF-8 encoded texts from PG have some examples of words with underscores around them to indicate emphasis.
  count(version, word) %>%
  group_by(version) %>%
  mutate(proportion = n / sum(n)) %>%
  select(-n) %>%
  spread(version, proportion) %>% #Spread and gather to reshape data frame, so that it's just what we need for
  gather(version, proportion, 'Hobbit Book') #plotting and comparing 


#Now we plot: 
library(scales)

#expect warning about rows with missing values being removed 
ggplot(frequency, aes(x=proportion, y=`Hobbit Script`, color = abs(`Hobbit Script` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) + 
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  facet_wrap(~version, ncol = 2) +
  theme(legend.position = "none") +
  labs(y = "Hobbit Script", x = NULL)

#Correlation between the frequency of the book and the script
cor.test(data=frequency[frequency$version == "Hobbit Book",],
         ~proportion + `Hobbit Script`)

##Pearson's product-moment correlation
# data:  proportion and Hobbit Script
# t = 31.154, df = 2630, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
# 0.4907174 0.5465552
# sample estimates:
#      cor 
# 0.5191901 


#plotting the token frequencies:
##BOOK 
book_freq <-book_tokens %>%
  filter(n > 500) %>% # we need this to eliminate all the low count words
  mutate(word = reorder(word,n )) %>%
  ggplot(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()

print(book_freq)


##SCRIPT
script_freq <-script_tokens %>%
  filter(n > 700) %>% # we need this to eliminate all the low count words
  mutate(word = reorder(word,n )) %>%
  ggplot(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()

print(script_freq)


###########################################################
## SENTIMENT ANALYSIS                                    ##
###########################################################

##########
#BOOK#
##########

#BING Book Sentiment Analysis - positive vs negative. 
book_bing_sent <- book_df %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

   
book_bing_sent %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()


##NRC Book Sentiment Analysis - each sentiment in NRC. 
book_nrc_sent <- book_df%>%
  unnest_tokens(word, text)%>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  ungroup()


book_nrc_sent %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()


##Book Sentiment Analysis with Bigrams

#Bigrams 
book_bigrams <- book_df %>% 
  unnest_tokens(bigram, text, token = 'ngrams', n=2)

# This data structure is still a variation od tidy text format, as one token per row, but each token represents
## a bigram. 

#Couting and Filtering N-grams - examinign most common bigrams 
book_bigrams %>%
  count(bigram, sort = TRUE)

#Using separate() to split columns into multiple columns based on a delimiter. 
book_big_sep <- book_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

book_big_filt <- book_big_sep %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

#New bigram counts: 
book_big_count <- book_big_filt %>%
  count(word1, word2, sort = TRUE)

#Afinn analysis -  bipolar 
afinn <- get_sentiments('afinn')

not_words <- book_big_sep %>%
  filter(word1=='not') %>%
  inner_join(afinn, by = c(word2='word')) %>%
  count(word2, value, sort=TRUE) %>%
  ungroup()

not_words %>%
  mutate(contribution=n * value) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(word2, n * value, fill= n * value > 0)) +
  geom_col(show.legend = FALSE) +
  xlab("Words preceded by \"not\"") +
  ylab("Sentiment Value * Number of Ocurrences") +
  ggtitle("Book - Polar Sentiment of Words Preceded by Not") +
  coord_flip()
  

#############
#SCRIPT
############

#BING Script Sentiment Analysis - positive vs negative. 
script_bing_sent <- script_df %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()


script_bing_sent %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()



##NRC Script Sentiment Analysis - each sentiment in NRC. 
script_ncr_sent <- script_df%>%
  unnest_tokens(word, text)%>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  ungroup()


script_ncr_sent %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()


##Script Sentiment Analysis with Bigrams

#Bigrams 
script_bigrams <- script_df %>% 
  unnest_tokens(bigram, text, token = 'ngrams', n=2)

# This data structure is still a variation od tidy text format, as one token per row, but each token represents
## a bigram. 

#Couting and Filtering N-grams - examinign most common bigrams 
script_bigrams %>%
  count(bigram, sort = TRUE)

#Using separate() to split columns into multiple columns based on a delimiter. 
script_big_sep <- script_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

script_big_filt <- script_big_sep %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

#New bigram counts: 
script_big_count <- script_big_filt %>%
  count(word1, word2, sort = TRUE)

#Afinn analysis -  bipolar 
#afinn <- get_sentiments('afinn')

not_words <- script_big_sep %>%
  filter(word1=='not') %>%
  inner_join(afinn, by = c(word2='word')) %>%
  count(word2, value, sort=TRUE) %>%
  ungroup()

not_words %>%
  mutate(contribution=n * value) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(word2, n * value, fill= n * value > 0)) +
  geom_col(show.legend = FALSE) +
  xlab("Words preceded by \"not\"") +
  ylab("Sentiment Value * Number of Ocurrences") +
  ggtitle("Script - Polar Sentiment of Words Preceded by Not") +
  coord_flip()


##CLOUDS WITH SENTIMENTS 
library(reshape2)
library(wordcloud)
library( RColorBrewer)

#BOOK CLOUD
book_cloud <-  book_df %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("purple", "red"),
                   max.words=100,
                   fixed.asp = TRUE,
                   title.size = 1)


book_cloud2 <-  book_tokens %>%
  inner_join(get_sentiments("nrc")) %>%  #nrc=all flavors
  count(word, sentiment, sort=TRUE) %>% #token per sentiment
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud( max.words=100, 
                   scale = c(2, 2),
                   fixed.asp = TRUE,
                   title.size = 1)

#Script CLOUD
script_cloud <-  script_df %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("purple", "red"),
                   max.words=100,
                   fixed.asp = TRUE,
                   title.size = 1)

script_cloud2 <-  script_tokens %>%
  inner_join(get_sentiments("nrc")) %>%  #nrc=all flavors
  count(word, sentiment, sort=TRUE) %>% #token per sentiment
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud( max.words=100, 
                    scale = c(2, 2),
                    fixed.asp = TRUE,
                    title.size = 1)

####################################################
################# TF_IDF ##########################
###################################################

#Putting both book and script together 
combination <- bind_rows(
  mutate(book_tokens, version = "Book"),
  mutate(script_tokens, version = "Script"),

)

combination <- combination %>%
  bind_tf_idf(word, version, n)

#Graphical approach
combination %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(version) %>%
  top_n(15) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=version))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~version, ncol=2, scales="free")+
  coord_flip()

head(combination, n=50)
