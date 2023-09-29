library(tidyverse)
library(rvest)
library(magrittr)

# names <- c("Dwight Howard", "Lebron James", "Kevin Durant")
# url <- "https://en.wikipedia.org/wiki/"
# df <- data.frame(names=names, url=paste0(url, sub(" ", "_", names)))
# df

all_nba_data <- read_csv("data/all_nba_data.csv") %>% 
  group_by(player)%>%mutate(num = n())

player_df <- all_nba_data %>% 
  dplyr::filter(num>=600) %>% 
  select(player) %>% 
  unique() %>% 
  mutate(url = paste0("https://en.wikipedia.org/wiki/",sub(" ", "_", player)))

results = data.frame()

for(i in 1:length(player_df$player)){
  tryCatch({
    message('Scraping URL: ', player_df$url[i])
    player_html <- read_html(player_df$url[i])
    pictures <- player_html %>% html_element('.infobox-image img') %>% 
      html_attr('srcset')
    
    pictures <- str_split(string = pictures, pattern = " ")[[1]][1]
    
    #position <- player_html%>%html_element('.infobox-data a')%>%html_attr('href')%>%html_attr('title')
    
    bday <- player_html %>% html_element('.infobox-data .bday')%>%html_text()
    
    infobox <- player_html %>% html_nodes(".infobox") %>% html_table()
    infobox <- infobox[[1]] 
    infobox<- infobox[!apply(infobox == "", 1, all), ] %>%drop_na("X1")
    infobox <- infobox %>% dplyr::select("X1", "X2")%>%head(20)%>%tidyr::pivot_wider(names_from = "X1", values_from = "X2")
    player <- player_df$player[i]
    height <- infobox$`Listed height`; height <- gsub("(?<=\\()[^()]*(?=\\))(*SKIP)(*F)|.", "", height, perl=T)
    weight <- infobox$`Listed weight`; weight <- gsub("(?<=\\()[^()]*(?=\\))(*SKIP)(*F)|.", "", weight, perl=T)
    position <- infobox$Position
    data <- as.data.frame(c(player, pictures, position, bday, height, weight))
    data <- data %>% 
      mutate(values = c("player","pictures", "position", "bday", "height", "weight")) %>% 
      tidyr::pivot_wider(names_from = values, values_from = `c(player, pictures, position, bday, height, weight)`)
    
    results <- rbind(results, data)
    print(paste0("Succes: ", i, ", ", player_df$player[i]))
  }, error = function(e){cat("ERROR :", player_df$player[i], "\n")})
}

colnames(results)[which(names(results) == "names")] <- "player"

all_nba_data <- dplyr::left_join(x = all_nba_data, y = results, by="player")

write.csv(x = all_nba_data, file = "all_nba_data_v2.csv")
