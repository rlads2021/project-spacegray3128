library(stringr)

# dataframe of artists with dash
df_artists_dash <- data.frame(matrix(NA, ncol=0, nrow = 100))
for(i in 1:ncol(df_artists) ){
    v <- as.vector(unlist(df_artists[ ,i]))
    test0 <- gsub("[[:punct:]]| |feat.|featuring", "-", v)
    test1 <- gsub("-+", "-", test0)
    artist_dash <- gsub("^-|-$", "", test1)
    
    print(ncol(df_artists_dash))
    df_artists_dash[ , ncol(df_artists_dash) + 1] <- artist_dash
    colnames(df_artists_dash)[ncol(df_artists_dash)] <- i+1971
}

# dataframe of titles with dash
df_titles_dash <- data.frame(matrix(NA, ncol=0, nrow = 100))
for(i in 1:ncol(df_titles) ){
    v <- as.vector(unlist(df_titles[ ,i])) 
    test0 <- gsub("[[:punct:]]| |feat.", "-", v)
    test1 <- gsub("-+", "-", test0)
    title_dash <- gsub("^-|-$", "", test1)
    
    print(ncol(df_titles_dash))
    df_titles_dash[ , ncol(df_titles_dash) + 1] <- title_dash                
    colnames(df_titles_dash)[ncol(df_titles_dash)] <- i+1971
}

