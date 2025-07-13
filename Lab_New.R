install.packages("tidytext")
install.packages("dplyr")
install.packages("SnowballC")
install.packages("wordcloud2")

library(tidytext)
library(dplyr)
library(SnowballC)
library(wordcloud2)

#data = search_tweets("Uber", n = 1000, include_rts = False, lang = "en")

Uber = read.csv("Uber.csv")

data = Uber[Uber$language == "en", ]

# Preprocessing

# Tokenize data
# Save it to word column using text columns
text_clean = unnest_tokens(data, word, tweet) # Split by words

# RM stop words
text_clean = anti_join(text_clean, stop_words)

# Stem
text_clean $stem = wordStem(text_clean$words) # Merge words with similar roots

# You can combine the three previous lines of code into one below:
text_clean = data %>% unnest_tokens(word, tweet) %>% anti_join(stop_words) %>% mutate(stem = wordStem(word))

# Word Cloud
wc1 = count(text_clean, word, sort = True)
wordcloud2(data = wc1, size = 1.6, color = 'random_dark')
# more customization in the wordcloud2 library

# TF, IDF
tf = count(text_clean, user_id, word)
tf_idf = bind_tf_idf(tf, term = word, document = user_id, n)

# Sentiment Analysis
sentiment = inner_join(text_clean, get_sentiments("bing")) # Positive negative
table(sentiment$sentiment)
table(sentiment$user_id, sentiment$sentiment)

# Library(textdata) They have other lists of words you can use