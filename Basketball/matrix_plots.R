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



final_df1<-final_df %>%
  dplyr::rename("Dunk rate" = "dunksRate",
                "Two pointer rate" = "twoPointersRate",
                "Three pointer rate" = "threePointersRate",
                "Two pointer success rate" = "twoPointerSuccess",
                "Three pointer success rate" = "threePointerSuccess") %>%
  tidyr::pivot_longer(cols = c("Dunk rate", 
                               "Two pointer rate", 
                               "Three pointer rate", 
                               "Two pointer success rate", 
                               "Three pointer success rate"), 
                      names_to = "metric", 
                      values_to = "matric_value") %>% 
  dplyr::filter(metric=="Three pointer rate") 





ggplot(final_df1, aes(x = season, y = factor(team,levels = rev(sort(unique(team)))), fill = matric_value)) + 
  ggplot2::geom_tile(colour="white", size=1.5, stat="identity") + 
  scale_fill_gradientn("",colours=pals::parula(100)) +
  #viridis::scale_fill_viridis(option="B") +
  #scale_y_continuous(breaks=1:length(unique_teams), labels=unique_teams)+
  ggplot2::xlab("") + 
  ggplot2::ylab("") +
  ggplot2::ggtitle(paste0("Shot evolution for all teams (", "Three pointer rate", ")")) +
  ggplot2::theme(
    plot.title = element_text(color="gray20",hjust=0,vjust=1, size=dropdown_size_filters),
    plot.background = element_rect(fill="white"),
    panel.background = element_rect(fill="white"),
    panel.border = element_rect(fill=NA,color="white", size=0.5, linetype="solid"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(), 
    axis.text = element_text(color="gray20", size=30),
    axis.text.y  = element_text(hjust=1, size=dropdown_size_filters),
    axis.text.x  = element_text(size=dropdown_size_filters),
    axis.title = element_text(size=dropdown_size_filters),
    legend.text = element_text(color="gray20", size=dropdown_size_filters),
    legend.background = element_rect(fill="white"),
    legend.position = "bottom",
    legend.title=element_blank()
  )+ggplot2::guides(fill = guide_colourbar(barwidth = 50, barheight = 2))
