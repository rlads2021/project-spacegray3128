library(dplyr)
library(httr)
library(rvest)
library(tibble)
library(ggplot2)

time <- vector()
nowtime <- 2020
for (i in 1:15) {
    time[i] <- as.character(nowtime - (i-1))
}
web <- paste0("charts/year-end/",time,"/hot-100-songs",collapse = ",")
webad <- strsplit(web,",")[[1]]

df_bill_year <- tibble()
for (i in seq_along(webad)) {
    html_bill <- GET("https://www.billboard.com/",path = webad[i])
    rank_bill <- content(html_bill) %>% 
        html_nodes(".ye-chart-item__rank") %>% html_text()
    rank_bill <- gsub("[\n]","",rank_bill)
    song_bill <- content(html_bill) %>%
        html_nodes(".ye-chart-item__title") %>% html_text()
    song_bill <- gsub("[\n]","",song_bill)    
    artist_bill <- content(html_bill) %>%
        html_nodes(".ye-chart-item__artist") %>% html_text()
    artist_bill <- gsub("[\n]","",artist_bill)
    df_bill <- tibble(rank = rank_bill[1:30], song = song_bill[1:30],
                      artist = artist_bill[1:30])
    df_bill <- df_bill %>% mutate(year = time[i])
    df_bill_year <- rbind(df_bill,df_bill_year)
}

count_by_showup <- df_bill_year %>%
    group_by(artist) %>%
    summarise(showup_time_year = n()) 
count_by_showup <- arrange(count_by_showup,desc(showup_time_year))
    
count_by_year <- df_bill_year %>%
    group_by(year) %>%
    summarise(showup_by_year = n())
count_by_year <- arrange(count_by_year,desc(showup_by_year))


ggplot(data = count_by_showup[1:15,]) +
    geom_bar(mapping = aes(x = artist, y = showup_time_year, fill = artist),
             stat = "identity",position = "stack", width = 0.55) +
    theme(text = element_text(size=5)) +
    labs(x = "十大歌手", y = "總進榜數(年度榜)",title = "15年間進榜數最多歌手")





    




















