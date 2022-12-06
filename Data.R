library(dplyr)

#1. Load and merge the datasets keeping all information available for the dates in which there is a measurement in “fx.csv”.
speeches <- read.csv("speeches.csv",
               sep = "|",
               header = TRUE) #1426 obs
length(unique(speeches$date)) #1113 date of records
speeches <- speeches[!is.na(speeches$contents), c ('date','contents')] #1426 obs
speeches <- speeches %>% 
            group_by(date) %>%    
            summarise(contents = paste(contents, collapse = " ")) #1113 obs

fx <- read.csv("fx.csv",
               sep = ",",
               header = TRUE,
               skip = 4)
colnames(fx) <- c("date", "exchange_rate")

df <- fx %>% left_join(speeches)
df$exchange_rate <- as.numeric(df$exchange_rate)
df$date <- as.Date(df$date)

#2. Remove entries with obvious outliers or mistakes, if any.

plot(df$date, df$exchange_rate, type ='l', xlab ="date", ylab ="EUR/USD reference exchange rate")

summary(df)

#3.Handle missing observations for the exchange rate, if any. 

library(zoo)

df$exchange_rate <- na.locf(df$exchange_rate, fromLast = TRUE)

#4.Calculate the exchange rate return.

df$return <- c(diff(df$exchange_rate)/df$exchange_rate[-1], NA)

df$goodnews <- as.numeric(df$return > 0.5/100)
df$badnews <- as.numeric(df$return < 0.5/100)

#5. Remove the entries for which contents is NA

library(tidyr)
df <- df %>% drop_na(contents)

#5a/5b Generate and store “good_indicators” and “bad_indicators”

library(stopwords)
stop_words <- stopwords()

library(text2vec)

get_word_freq <- function(contents, stop_words, num_words) {
  words <- unlist(lapply(contents, word_tokenizer))
  words <- tolower(words)
  freq <- table(words)
  freq <- freq[!names(freq) %in% stop_words]
  names(freq[order(-freq)][1:num_words])
}

good_news_contents <- df$contents[df$goodnews==1]    # contents related to 137 "good_news" 
bad_news_contents <- df$contents[df$badnews==1]      # contents related to 143 "bad_news"

good_indicators <- get_word_freq(good_news_contents, stop_words, num_words = 20)
bad_indicators <- get_word_freq(bad_news_contents, stop_words, num_words = 20)

good_indicators
bad_indicators

write.table(good_indicators, file="good_indicators.csv", sep=",")
write.table(bad_indicators, file="bad_indicators.csv", sep=",")
