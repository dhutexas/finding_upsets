# plotting and theme

############################
# Custom Theme for Article #
############################

matchups_theme <- theme_minimal(base_family = "Helvetica", base_size = 13) +
  theme(axis.text = element_text(size = rel(1.2)),
        strip.text = element_text(),
        axis.ticks.x = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "bottom",
        legend.text.align = 0,
        legend.background = element_blank(),
        legend.title = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(size = rel(1.2),
                                   color = "#222222"),
        plot.title = element_text(size = rel(2.0),
                                  face = "bold", 
                                  color = "#222222"),
        plot.subtitle = element_text(size = rel(1.4),
                                     margin = margin(9, 0, 9, 0)))


#################
# Plot Matchups #
#################

# **SAVE PLOTS @ 1100w x 500h** 

# Establish Color Patterns 
# defense = red #DC244B, #AF1D3C
# offense = yellow #F6CB52, #F3B816
# ratings = green #52A1A3, #76C8B1, #50B99B
# avg_score = grey #778da9
cols <- c("avg_score" = '#778da9',
          `Defense: KenPom` = "#DC244B", `Defense: Massey` = "#AF1D3C", 
          `Offense: KenPom` = "#F6CB52", `Offense: Massey` = "#F3B816",
          `Rating: ELO` = '#52A1A3', `Rating: Massey` = '#76C8B1', `Rating: Massey Power` = '#50B99B')


# function to plot all matchups in a round
plot_round_matchups <- function(round_name, title) {
  matchups %>%
    filter(round == round_name) %>%
    select(seed_team, slot, avg_score,lower_ci, upper_ci,
           `Rating: ELO`,`Rating: Massey Power`,`Rating: Massey`,
           `Offense: KenPom`,`Offense: Massey`,`Defense: KenPom`,`Defense: Massey`) %>%
    pivot_longer(cols = c(`Rating: ELO`:`Defense: Massey`),
                 values_to = 'rating',
                 names_to = 'metric') %>%
    ggplot(aes(x = avg_score, y = reorder(seed_team, avg_score))) +
    geom_point(aes(x = avg_score, y = seed_team), 
               size = 2.75) +
    geom_errorbarh(aes(xmin = lower_ci,
                       xmax = upper_ci),
                   height = 0.3,
                   alpha = 0.25) +
    geom_point(aes(x = rating, y = seed_team, 
                   color = metric),
               alpha = 0.5,
               size = 2.5) +
    scale_colour_manual(values = cols) +
    geom_vline(xintercept=0, linetype = 'dashed') +
    facet_wrap(~slot, 
               scales = 'free',
               ncol = 2) +
    scale_x_continuous(limits=c(-3,3), breaks = c(-3, -2, -1, 0, 1, 2, 3)) +
    labs(title = title,
         subtitle = 'Matchups Between Seeds',
         caption = 'Note: x-axis represents standard deviations +/- average team') +
    matchups_theme -> matchups_plot
  
  return(matchups_plot)
}

# example
plot_round_matchups(round_name = 'R3', 'Round Three - Sweet Sixteen')


# function to plot matchup of specific seeds
plot_specific_matchups <- function(round_name, low_seed, high_seed, title) {
  
  matchups %>%
    filter(round == round_name) %>%
    filter(seed_int == low_seed | seed_int == high_seed) %>%
    select(seed_team, slot, avg_score,lower_ci, upper_ci,
           `Rating: ELO`,`Rating: Massey Power`,`Rating: Massey`,
           `Offense: KenPom`,`Offense: Massey`,`Defense: KenPom`,`Defense: Massey`) %>%
    pivot_longer(cols = c(`Rating: ELO`:`Defense: Massey`),
                 values_to = 'rating',
                 names_to = 'metric') %>%
    ggplot(aes(x = avg_score, y = reorder(seed_team, avg_score))) +
    geom_point(aes(x = avg_score, y = seed_team), 
               size = 2.75) +
    geom_errorbarh(aes(xmin = lower_ci,
                       xmax = upper_ci),
                   height = 0.3,
                   alpha = 0.25) +
    geom_point(aes(x = rating, y = seed_team, 
                   color = metric),
               alpha = 0.5,
               size = 2.5) +
    scale_colour_manual(values = cols) +
    geom_vline(xintercept=0, linetype = 'dashed') +
    facet_wrap(~slot, 
               scales = 'free',
               ncol = 2) +
    scale_x_continuous(limits=c(-3,3), breaks = c(-3, -2, -1, 0, 1, 2, 3)) +
    labs(title = title,
         subtitle = 'Matchups Between Seeds',
         caption = 'Note: x-axis represents standard deviations +/- average team') +
    matchups_theme -> matchups_pair_plot
  
  return(matchups_pair_plot)
}

# example
plot_specific_matchups(round_name = 'R1', low_seed = '2', high_seed = '15', 
                       title = 'Two vs. Fifteen Seed Matchups, Round One')
