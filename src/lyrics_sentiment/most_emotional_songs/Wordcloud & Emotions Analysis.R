

#create wordcloud graphs
term_frequency <- rowSums(clean_m)
term_frequency <- sort(term_frequency, decreasing = TRUE)
term_frequency[1:10]

figPath <- "C:/Users/brian/Desktop/R final project/icon/music.png"
#draw the graphs of frequency words
plot1 <- data.frame(names(term_frequency), term_frequency) %>%
    arrange(desc(term_frequency)) %>%
    top_n(200) 
wordcloud2(plot1, size = 0.25, color="random-light",shape = 'triangle',backgroundColor = "black")  
wordcloud2(plot1 , figPath = figPath, size = 3,color = "green", backgroundColor = "black")


#draw the graphs of artist
top100_artist <- song_data %>%
    group_by(artist) %>%
    summarise(total = length(song)) %>%
    arrange(desc(total)) %>%
    top_n(100) 

wordcloud(as.character(top100_artist$artist),
          as.numeric(top100_artist$total), min.freq = 1,
          max.words=200, random.order=FALSE, random.color = FALSE, rot.per=0.3, scale = c(1.1,0.1), 
          colors=brewer.pal(11, "Spectral"))


#Sentiment Analytics
nrc <- get_sentiments("nrc")
temp_table <- data.frame(word = names(term_frequency), word_count = term_frequency) %>%
    inner_join(nrc)

temp_table %>% 
    group_by(sentiment) %>%
    top_n(10, word_count) %>%
    ungroup() %>%
    mutate(word = reorder(word,word_count)) %>%
    ggplot(aes(x = word, 
               y = word_count, fill = sentiment)) +
    geom_col() +
    facet_wrap(~sentiment, scales = "free")+
    coord_flip() +
    theme(axis.text.y = element_text(size = 7), 
          axis.text.x = element_text(size = 5)) 


#the most positive and negative songs in list 
unnest_tidy <- song_data %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words)

#positive
positive_song <- unnest_tidy %>% 
    inner_join(get_sentiments("bing")) %>%
    group_by(song, sentiment) %>%
    summarize(score = n()) %>%
    spread(sentiment, score) %>% 
    mutate_at(vars(negative:positive), funs(replace_na(., 1))) %>%
    ungroup()%>%
    mutate(positiveratio = positive/negative, 
           song = reorder(song,positiveratio))

ggplot(top_n(positive_song, 30),aes(x = song, y = positiveratio)) +
    geom_point(color="#66b2b2", size = 3) +
    geom_segment(aes(x=song, xend=song, 
                     y=min(positiveratio), yend=max(positiveratio)),
                 linetype="dashed", size=0.1, color ="grey") +
    coord_flip() +
    labs(title = "Most Positive Songs", 
         subtitle = "Positivity calculated based on ratio of positive to negative words")+
    theme_minimal() +
    theme(text = element_text(size = 10),
          plot.title = element_text(size = 16, color = "#ff5a5f", face = "bold",margin = margin(b = 7)),
          plot.subtitle = element_text(size = 10, color =                     "darkslategrey", margin = margin(b = 7)))

#negative
negative_song <- unnest_tidy %>% 
    inner_join(get_sentiments("bing")) %>%
    group_by(song, sentiment) %>%
    summarize(score = n()) %>%
    spread(sentiment, score) %>% 
    mutate_at(vars(negative:positive), funs(replace_na(., 1))) %>%
    ungroup()%>%
    mutate(negativeratio = negative/positive,
           song = reorder(song,negativeratio))

ggplot(top_n(negative_song, 30),aes(x = song, y = negativeratio)) +
    geom_point(color="tomato", size = 3) +
    geom_segment(aes(x=song, xend=song,       
                     y=min(negativeratio),yend=max(negativeratio)), 
                 linetype="dashed", size=0.1, color = "grey") +
    coord_flip() +
    labs(title = "Most Negative Songs", 
         subtitle = "Negativity calculated based on ratio of negative to positive words")+
    theme_minimal() +
    theme(text = element_text(size = 10),
          plot.title = element_text(size = 16, color = "#ff5a5f", face = "bold",margin = margin(b = 7)),
          plot.subtitle = element_text(size = 10, color = "darkslategrey", margin = margin(b = 7)))

# correlation for specific artist - take justin bieber for instance
section_cp<-songs%>%filter(artist=='Justin Bieber')%>%mutate(section = row_number() %/% 10) %>%
    filter(section >= 0) %>%
    unnest_tokens(word, text) %>%
    filter(!word %in% stop_words$word) %>%
    filter(nchar(word) > 4)