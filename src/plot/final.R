library(jiebaR)
library(tidytext)
library(dplyr)
library(stringr)
library(ggplot2)
library(wordcloud2)
library(wordcloud)







#import data
song_1970 <- read.csv("all_songs_1970s_.csv")
song_1980 <- read.csv("all_songs_1980s_.csv")
song_1990 <- read.csv("all_songs_1990s_.csv")
song_2000 <- read.csv("all_songs_2000s_.csv")
song_2010 <- read.csv("all_songs_2010s_.csv")
song_2020 <- read.csv("all_songs_2020s_.csv")











#合併、清除NULL
all_songs_raw <- bind_rows( song_1970,
                            song_1980,
                            song_1990,
                            song_2000,
                            song_2010,
                            song_2020)
all_songs <- all_songs_raw %>% filter(!Lyrics == "null") 










#decades
breaks <- c(1972,1980,1990,2000,2010,2022)
labels <- c("1970s", "1980s", "1990s", "2000s", "2010s~now")
all_songs$decade <- cut(all_songs$year,
                        breaks = breaks,
                        include.lowest = TRUE,
                        right = FALSE,
                        labels = labels)












#cleaning
cleand <- all_songs
#還原縮寫
clean <- function(x){
    x = gsub("won't", "will not", x)
    x = gsub("can't", "can not", x)
    x = gsub("n't", " not", x)
    x = gsub("'ll", " will", x)
    x = gsub("'re", " are", x)
    x = gsub("'ve", " have", x)
    x = gsub("'m", " am", x)
    x = gsub("'d", " would", x)
    x = gsub("'s", "", x)
    x = gsub('feelin|feelingg', 'feeling', x)
    x = gsub('tryin', 'trying', x)
    x = gsub('mothafucka', 'motherfucker', x)
    x = gsub('wanna', 'want to', x)
    x = gsub('dat', 'that', x)
    return(x)
}
#清除歌詞資料裡面[Verse]、[Bridge]之類的非歌詞元素
removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]", " ", x)

cleand$Lyrics = all_songs$Lyrics %>%
    gsub(pattern = '\\[[^][]*]', replacement = ' ') %>%
    tolower() %>%
    clean() %>% 
    removeSpecialChars() %>%
    gsub(pattern = '[[:punct:]|[:digit:]]', replacement = ' ')











#tidying-lyrics
#making stopwords list
myStopwords = c('ooh', 'oooh', 'oh', 'ow', 'uh', 'baby', 'babi', 'bebe', 
                'yeah', 'yeh', 'ye', 'yes', 'ya', 'eh', 'da', 'cardi', 'se',
                'ayy', 'ah', 'yo', 'o', 'bum', 'na', 'la', 'ai', 'ba', 'hey','chorus',
                'da','yo','dr','aah','mckenzie','2006','yuhh','yurr','aaaaaaaaaaaaahhhhhhh',
                'aaahhs','aaaaahh', 'aaaaaaaaaaaaaah', 'uhuh',
                'gon','gonna','woo','badoomboom','badoom','boom','bass','hmm', 'mmm', 'mmhmm')
df_tidy <- cleand %>% 
    ungroup() %>% 
    unnest_tokens(word, Lyrics) %>%
    distinct() %>%
    filter(!word %in% myStopwords & decade != 'NA') %>%
    anti_join(stop_words) %>%
    filter(nchar(word) > 2) %>% 
    select(year, Artist, Title, word, decade)











#算字數
plt_wf <- df_tidy %>%
    group_by(Artist, Title, decade) %>%
    count()  %>%
    ggplot(aes(x = decade, y = n, fill = decade)) + 
    geom_boxplot(show.legend = F) +
    labs(x = '', y = 'Word Count', title = 'Word frequency in relation to decades') +
    theme_bw()
#算平均
df_mean_length <- df_tidy %>%
    group_by(Artist, Title, decade) %>%
    count()  %>%
    group_by(decade)%>%
    summarise(mean = mean(n))




#算詞頻
unigram_tidy <- df_tidy %>%
    group_by(word) %>%
    count() %>% 
    ungroup () %>%
    arrange(desc(n))

plt_unigram <- wordcloud2(data = unigram_tidy[1:100, ], size = 1, color = brewer.pal(7, 'Dark2'))










#timeless-lyrics
timeless_words <- df_tidy %>%
    group_by(decade) %>%
    count(word, decade, sort = TRUE) %>%
    slice(seq_len(7)) %>%
    ungroup() %>%
    arrange(decade, n) %>%
    mutate(row = row_number())

plt_timeless <- timeless_words %>%
    ggplot(aes(row, n, fill = decade)) +
    geom_col() +
    labs(title = "Timeless words",
         x = NULL,
         y = NULL) +
    theme_bw() +
    theme(legend.position = "None") +
    facet_wrap(~decade, scales = "free", ncol = 5) +
    scale_x_continuous(breaks = timeless_words$row,
                       labels = timeless_words$word) +
    theme(axis.text.x = element_blank()) +
    coord_flip() 










#sentiment
#by year
df_ratio_year <- df_tidy %>%
    inner_join(get_sentiments("bing")) %>%
    group_by(year, sentiment) %>%
    summarize(score = n()) %>%
    spread(sentiment, score) %>%
    ungroup() %>%
    mutate(ratio = positive / (positive + negative),
           year = reorder(year, ratio))

df_ratio_year$year = factor(df_ratio_year$year, levels = c(1972:2021))
plt_positive_ratio_yr <- df_ratio_year %>%
    ggplot(aes(year, ratio)) +
    geom_point() +
    geom_line(aes(group="")) +
    labs(title = "Positive ratio of songs") +
    theme(axis.text.x = element_text(size = 7, angle = 45) )


#by decade
df_ratio_decade <- df_tidy %>%
    inner_join(get_sentiments("bing")) %>%
    group_by(decade, sentiment) %>%
    summarize(score = n()) %>%
    spread(sentiment, score) %>%
    ungroup() %>%
    mutate(ratio = positive / (positive + negative),
           decade = reorder(decade, ratio))

df_ratio_decade$decade = factor(df_ratio_decade$decade, levels = c("1970s", "1980s", "1990s", "2000s", "2010s~now") )
plt_positive_ratio_dc <- df_ratio_decade %>%
    ggplot(aes(decade, ratio)) +
    geom_point() +
    geom_line(aes(group="")) +
    labs(title = "Positive ratio of songs") +
    theme(axis.text.x = element_text(size = 10) )

#by artist
ratio_song_artist <- df_tidy %>% 
    inner_join(get_sentiments("bing")) %>%
    group_by(Artist, sentiment) %>%
    summarize(score = n()) %>%
    spread(sentiment, score) %>% 
    ungroup() %>%
    mutate(ratio = positive / (positive + negative),
           Artist = reorder(Artist, ratio))
#positive
plt_positive_artists <-ratio_song_artist %>%
    top_n(20) %>%
    group_by(Artist)%>%
    summarise(ratio = ratio/n()) %>%
    ggplot(aes(x = Artist, y = ratio)) +
    geom_point(color = "blue", size = 4) +
    coord_flip() +
    labs(title = "Top 20 Most Positive Artists",
         x = "",
         caption = "ratio = positive to positive and negative words jointly") +
    theme_minimal() +
    theme(plot.title = element_text(size = 16, face = "bold"),
          panel.grid = element_line(linetype = "dashed", color = "darkgrey", size = .5))
#negative
plt_negative_artists <-ratio_song_artist %>%
    mutate(ratio = 1 - ratio, 
           Artist = reorder(Artist, ratio)) %>%
    top_n(20) %>%
    group_by(Artist)%>%
    summarise(ratio = ratio/n()) %>%
    ggplot(aes(x = Artist, y = ratio)) +
    geom_point(color = "red", size = 4) +
    coord_flip() +
    labs(title = "Top 20 Most Negative Artists",
         x = "",
         caption = "ratio = negative to positive and negative words jointly") +
    theme_minimal() +
    theme(plot.title = element_text(size = 16, face = "bold"),
          panel.grid = element_line(linetype = "dashed", color = "darkgrey", size = .5))













#title
cleand_ttl <- all_songs
#cleaning titles
cleand_ttl$Title = all_songs$Title %>%
    gsub(pattern = '\\[[^][]*]', replacement = ' ') %>%
    tolower() %>%
    clean() %>% 
    removeSpecialChars() %>%
    gsub(pattern = '[[:punct:]|[:digit:]]', replacement = ' ')
#tidying titles
df_tidy_title <- cleand_ttl %>%
    ungroup() %>%
    unnest_tokens(word, Title) %>%
    distinct() %>%
    filter(!word %in% myStopwords ) %>%
    anti_join(stop_words) %>%
    filter(nchar(word) > 2) %>%
    select(year,Artist,word,decade)



#算詞頻 title
unigram_tidy_title <- df_tidy_title %>%
    group_by(word) %>%
    count() %>% 
    ungroup () %>%
    arrange(desc(n))

plt_unigram_title <- wordcloud2(data = unigram_tidy[1:100, ], size = 1, color = brewer.pal(7, 'Dark2'))






#timeless title
timeless_words_title <- df_tidy_title %>%
    group_by(decade) %>%
    count(word, decade, sort = TRUE) %>%
    slice(seq_len(7)) %>%
    ungroup() %>%
    arrange(decade, n) %>%
    mutate(row = row_number())

plt_timeless_title <- timeless_words_title %>%
    ggplot(aes(row, n, fill = decade)) +
    geom_col() +
    labs(title = "Timeless words of Titles of Songs",
         x = NULL,
         y = NULL) +
    theme_bw() +
    theme(legend.position = "None") +
    facet_wrap(~decade, scales = "free", ncol = 5) +
    scale_x_continuous(breaks = timeless_words_title$row,
                       labels = timeless_words_title$word) +
    #theme(axis.text.x = element_blank()) +
    coord_flip() 






#seniment title
#by year
df_ratio_year_title <- df_tidy_title %>%
    inner_join(get_sentiments("bing")) %>%
    group_by(year, sentiment) %>%
    summarize(score = n()) %>%
    spread(sentiment, score) %>%
    ungroup() %>%
    mutate(ratio = positive / (positive + negative),
           year = reorder(year, ratio))

df_ratio_year_title$year = factor(df_ratio_year_title$year, levels = c(1972:2021))
plt_positive_ratio_title_yr <- df_ratio_year_title %>%
    ggplot(aes(year, ratio)) +
    geom_point() +
    geom_line(aes(group="")) +
    labs(title = "Positive ratio of Names of songs") +
    theme(axis.text.x = element_text(size = 7, angle = 45) )

#by decade
df_ratio_decade_title <- df_tidy_title %>%
    inner_join(get_sentiments("bing")) %>%
    group_by(decade, sentiment) %>%
    summarize(score = n()) %>%
    spread(sentiment, score) %>%
    ungroup() %>%
    mutate(ratio = positive / (positive + negative),
           decade = reorder(decade, ratio))

df_ratio_decade$decade = factor(df_ratio_decade$decade, levels = c("1970s", "1980s", "1990s", "2000s", "2010s~now") )
plt_positive_ratio_dc_title <- df_ratio_decade_title %>%
    ggplot(aes(decade, ratio)) +
    geom_point() +
    geom_line(aes(group="")) +
    labs(title = "Positive ratio of Names of songs") +
    theme(axis.text.x = element_text(size = 10) )























#TF-IDF
tfidf_words_decade <- df_tidy %>%
    count(decade, word, sort = TRUE) %>%
    ungroup() %>%
    bind_tf_idf(word, decade, n) %>%
    arrange(desc(tf_idf))

top_tfidf_words_decade <- tfidf_words_decade %>%
    group_by(decade) %>%
    slice(seq_len(8)) %>%
    ungroup() %>%
    arrange(decade, tf_idf) %>%
    mutate(row = row_number())

plt_tfidf <- top_tfidf_words_decade %>%
    ggplot(aes(x = row, tf_idf, fill = decade)) +
    geom_col(show.legend = NULL) +
    labs(x = NULL, y = "TF-IDF") +
    ggtitle("Important Words using TF-IDF by Decade") +
    theme_bw() +
    facet_wrap(~decade,
               ncol = 2, nrow = 3,
               scales = "free") +
    scale_x_continuous(
        breaks = top_tfidf_words_decade$row,
        labels = top_tfidf_words_decade$word) +
    coord_flip() 