#song_name & artist_name are transformed into X-X format to match the URL
song_name <- lapply(df_bill_year$song,function(x){
    k <- paste(strsplit(x," ")[[1]],collapse = "-")
    return(k)
})

artist_name <- lapply(df_bill_year$artist,function(x){
    m <- paste(strsplit(x," ")[[1]],collapse = "-")
    return(m)
})

song_path <- vector()
for (i in 1:nrow(df_bill_year)) {
    song_p <- paste0("lyrics/",artist_name[[i]],"/",song_name[[i]])
    song_path <- c(song_path,song_p)}


#web crawler for the song's lyrics
lyrics_analyze <- vector()

for (song in song_path) {
    
    tryCatch({html_lyrics <- paste0("https://www.musixmatch.com/" 
                                    , path = song , collapse = "")
    lyrics <- read_html(html_lyrics) %>%
        html_nodes(".lyrics__content__ok") %>% html_text()
    lyrics <- gsub("\n","",lyrics)
    }
    , warning = function(c) {"error"
        lyrics = " NA "}
    , error = function(c)  {"error"
        lyrics = " NA "} )
    
    
    
    lyrics_com <- vector()
    for (i in 1:length(lyrics)) {
        lyrics_com <- paste(lyrics_com,lyrics[i],collapse = " ")
    }
    lyrics_analyze <- c(lyrics_analyze,lyrics_com)
}


#combine df_bill_year with lyrics obtained from musixmatch
df_bill_lyrics <- df_bill_year %>% mutate(text = lyrics_analyze) %>% filter(text != " NA ")

doc_id <- seq(1,nrow(df_bill_lyrics),by = 1)
song_data <- df_bill_lyrics %>% mutate(doc_id = doc_id) %>% select(doc_id,text,artist,song,year,rank)

