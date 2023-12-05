### Example Viridis Plot

library(ggplot2)
library(viridis)
library(season)
library(gridExtra)

get_all_teams <- function(team, df){
  
  get_avg_team <- function(selected_season, selected_team, data){
    output_df <- data %>% 
      dplyr::filter(team_name == selected_team, season == selected_season) %>%
      dplyr::summarise(dunksRate = (sum(distance < 2) / nrow(.)) * 100,
                       twoPointersRate = ((sum(shot_type == "two_pointer")) / (nrow(.))) * 100,
                       threePointersRate = ((sum(shot_type == "three_pointer")) / (nrow(.))) * 100,
                       twoPointerSuccess = ((sum(made == T 
                                              & shot_type == "two_pointer")) / (sum(shot_type == "two_pointer"))) * 100,
                       threePointerSuccess = ((sum(made == T 
                                                 & shot_type == "three_pointer")) / (sum(shot_type == "three_pointer"))) * 100,
                       season = selected_season,
                       team = selected_team)
    
    return(output_df)
  }
  
  seasons <- unique(df$season)

  compare_df <- 
    lapply(seasons, get_avg_team, selected_team = team, data = df) %>%
    do.call(rbind, .) %>%
    unnest(everything()) %>%
    as.data.frame()
  
  return(compare_df)
}

unique_teams <- unique(all_nba_data$team_name)
final_df <- 
  lapply(unique_teams, get_all_teams, df = all_nba_data) %>%
  do.call(rbind, .) %>%
  unnest(everything()) %>%
  as.data.frame()



ggplot(final_df, aes_string("season", "team", fill = "threePointerSuccess")) + 
  geom_tile(colour="white", size=1.5, stat="identity") + 
  scale_fill_gradientn("", colours=pals::parula(100)) +
  # scale_y_discrete(breaks=1:length(unique_teams), labels=unique_teams)+
  xlab("") + 
  ylab("") +
  ggtitle("Shot evolution for all teams") +
  theme(
    plot.title = element_text(color="gray20",hjust=0,vjust=1, size=rel(2)),
    plot.background = element_rect(fill="white"),
    panel.background = element_rect(fill="white"),
    panel.border = element_rect(fill=NA,color="white", size=0.5, linetype="solid"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(), 
    axis.text = element_text(color="gray20", size=rel(1.5)),
    axis.text.y  = element_text(hjust=1),
    legend.text = element_text(color="gray20", size=rel(1.3)),
    legend.background = element_rect(fill="white"),
    legend.position = "bottom",
    legend.title=element_blank()
  )+guides(fill = guide_colourbar(barwidth = 50, barheight = 2))
# Sorter alfabetisk
# Skriv værdi i brackets, men vælg font/size mm. nøjsomt (tænk på luminance)
# Lav legend bredere
# Perula color scale kan han godt lide

