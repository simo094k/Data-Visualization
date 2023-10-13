library(tidyverse)
library(rvest)
library(magrittr)


team_mapping <- data.frame(
  team = c("BOS", "CLE", "WAS", "DAL", "LAC", "LAL", "HOU", "POR", "IND", "ATL",
             "CHA", "UTA", "DEN", "GSW", "PHO", "DET", "MEM", "NYK", "MIA", "NJN",
             "MIN", "SAC", "OKC", "PHI", "ORL", "NOH", "SAS", "TOR", "CHI",
             "MIL", "BRK", "NOP", "CHO"),
  team_name = c("Boston Celtics", "Cleveland Cavaliers", "Washington Wizards", "Dallas Mavericks",
                "Los Angeles Clippers", "Los Angeles Lakers", "Houston Rockets", "Portland Trail Blazers",
                "Indiana Pacers", "Atlanta Hawks", "Charlotte Hornets", "Utah Jazz", "Denver Nuggets",
                "Golden State Warriors", "Phoenix Suns", "Detroit Pistons", "Memphis Grizzlies",
                "New York Knicks", "Miami Heat", "Brooklyn Nets", "Minnesota Timberwolves",
                "Sacramento Kings", "Oklahoma City Thunder", "Philadelphia 76ers", "Orlando Magic",
                "New Orleans Hornets", "San Antonio Spurs", "Toronto Raptors", "Chicago Bulls",
                "Milwaukee Bucks", "Brooklyn Nets", "New Orleans Pelicans", "Charlotte Hornets")
)

all_nba_data <- left_join(x = all_nba_data, y = team_mapping, by="team")

team_df <- all_nba_data %>% 
  select(team_name) %>% 
  unique() %>% 
  mutate(url = paste0("https://en.wikipedia.org/wiki/",gsub(" ", "_", team_name)))

results = data.frame()

for(i in 1:length(team_df$team_name)){
  tryCatch({
    message('Scraping URL: ', team_df$url[i])
    team_html <- read_html(team_df$url[i])
    pictures_team <- team_html %>% html_element('.infobox-image img') %>% 
      html_attr('srcset')
    
    pictures_team <- str_split(string = pictures_team, pattern = " ")[[1]][1]
    team_name <- team_df$team_name[i]
    
    data <- as.data.frame(c(team_name, pictures_team))
    data <- data %>% 
      mutate(values = c("team_name","pictures_team")) %>% 
      tidyr::pivot_wider(names_from = values, values_from = `c(team_name, pictures_team)`)
    
    results <- rbind(results, data)
    print(paste0("Succes: ", i, ", ", team_df$team_name[i]))
  }, error = function(e){cat("ERROR :", team_df$team_name[i], "\n")})
}

#colnames(results)[which(names(results) == "names")] <- "player"

all_nba_data <- dplyr::left_join(x = all_nba_data, y = results, by="team_name")

#write.csv(x = tst, file = "players_no_position.csv")



