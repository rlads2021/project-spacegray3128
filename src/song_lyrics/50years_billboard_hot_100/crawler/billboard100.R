library(httr)
library(rvest)
library(stringr)


year_websites <- c()
i = 0
for(year in 1972:2021){
    i <- i + 1
    year_websites[i] <- paste0("https://www.billboard.com/charts/hot-100/", year, "-06-05")
}
head(year_websites)


#title
df_titles <- data.frame(matrix(NA, ncol=0, nrow = 100))
for(year in 1972:2021 ){
    resp <- GET( paste0("https://www.billboard.com/charts/hot-100/", year, "-06-05") )
    while(resp$status_code != 200){
        resp <- GET( paste0("https://www.billboard.com/charts/hot-100/", year, "-06-05") )
    }
    html <- content(resp)
    songs <- html %>%
        html_nodes("ol.chart-list__elements")
    title <- songs %>%
        html_nodes(".chart-element__information__song") %>%
        html_text() %>%
        trimws()
    
    df_titles[ , ncol(df_titles) + 1] <- title
    colnames(df_titles)[ncol(df_titles)] <- year
}


#artist
df_artists <- data.frame(matrix(NA, ncol=0, nrow = 100))
for(year in 1972:2021 ){
    resp <- GET( paste0("https://www.billboard.com/charts/hot-100/", year, "-06-05") )
    while(resp$status_code != 200){
        resp <- GET( paste0("https://www.billboard.com/charts/hot-100/", year, "-06-05") )
    }
    html <- content(resp)
    songs <- html %>%
        html_nodes("ol.chart-list__elements")
    artist <- songs %>%
        html_nodes(".chart-element__information__artist") %>%
        html_text() %>%
        trimws()
    
    df_artists[ , ncol(df_artists) + 1] <- artist                
    colnames(df_artists)[ncol(df_artists)] <- year
}



