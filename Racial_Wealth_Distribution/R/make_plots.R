##%######################################################%##
#                                                          #
####            Analyze data and make plots             ####
#                                                          #
##%######################################################%##

#   ____________________________________________________________________________
#   Load data                                                               ####
set.seed(100)
library(survey)
library(magrittr)
library(dplyr)
library(mitools)
library(ggplot2)
library(ggthemes)
library(here)
library(hrbrthemes)
data_directory <- paste(dirname(here()), "data", sep = "/")
write_data_directory <- here("data")
main_data <- readRDS(paste(data_directory, "scf 2016.rds", sep = "/"))
replicate_data <- readRDS(paste(data_directory, "scf 2016 rw.rds", sep = "/"))

#   ____________________________________________________________________________
#   Set up survey                                                           ####

main_design <-
  svrepdesign(
    weights          = ~wgt ,
    repweights       = replicate_data[ , -1 ] ,
    data             = imputationList( main_data ) ,
    scale            = 1 ,
    rscales          = rep( 1 / 998 , 999 ) ,
    mse              = TRUE ,
    type             = "other" ,
    combined.weights = TRUE
  )


# Removing race == 5 subsets to White, Latino, and Black respondents
filtered_design <- subset(main_design, race != 5)

#   ____________________________________________________________________________
#   Calculate quantiles                                                     ####

quantile_vector <- seq(0, 1, .01)

##  ............................................................................
##  Calculate overall wealth quantiles                                      ####

total_wealth_pctiles <- quantile_vector %>%
  lapply(function(x){lodown:::scf_MIcombine(
    with( main_design , svyquantile( ~ networth , x ) )
  )}
  ) %>%
  lapply(function(x) x$coefficients) %>%
  unlist %>%
  data.frame(total_ntile = quantile_vector * 100, total_ntile_value = .)

readr::write_csv(total_wealth_pctiles, paste(write_data_directory, "overall_percentiles.csv", sep = "/"))

##  ............................................................................
##  Calculate wealth quantiles by race                                      ####


racial_wealth_pctiles <- lodown:::scf_MIcombine(
  with( filtered_design ,
        svyby(
          ~ networth , ~ as.factor(race)  , svyquantile , quantile_vector, se=FALSE
        ) ) )
racial_wealth_pctiles %<>%
  .$coefficients %>%
  data.frame(variable = names(.), racial_ntile_value = .) %>%
  tidyr::separate(variable, c("race", "race_ntile"), sep = ":V")

racial_wealth_pctiles %<>% dplyr::mutate(race = forcats::fct_recode(race,
                                           White = "1",
                                           Black = "2",
                                           Latino = "3"))

racial_wealth_pctiles$total_ntile <- findInterval(racial_wealth_pctiles$racial_ntile_value, total_wealth_pctiles$total_ntile_value)

readr::write_csv(racial_wealth_pctiles, paste(write_data_directory, "racial_percentiles.csv", sep = "/"))

#   ____________________________________________________________________________
#   Generate plots                                                          ####

plot_directory <- here("plots")

##  ............................................................................
##  Plot racial percentile vs overall percentile                            ####

racial_plot <- racial_wealth_pctiles %>%
  filter(between(total_ntile, 1, 99)) %>%
  ggplot() +
  geom_smooth(aes(x = as.numeric(race_ntile), y = total_ntile, color = race),
              se=FALSE,
              span = .1) +
  geom_segment(data=data.frame(x=0, y=0, xend = 100, yend=100),
               aes(x = x, y = y, xend = xend, yend = yend, color="Equality"),
               linetype = "dashed", show.legend = TRUE) +
  scale_color_colorblind() +
  xlab("Racial Wealth Percentile") +
  ylab("Overall Wealth Percentile") +
  ggtitle("Racial Wealth Percentile by Overall Wealth Percentile (2016)") +
  labs(caption = "Source: Survey of Consumer Finances") +
  theme_ipsum(grid = "Y") +
  theme(legend.title = element_blank())

racial_plot_transparent <-
  racial_plot + theme(
    panel.background = element_rect(fill = "transparent") # bg of the panel
    , plot.background = element_rect(fill = "transparent") # bg of the plot
    , panel.grid.major = element_blank() # get rid of major grid
    , panel.grid.minor = element_blank() # get rid of minor grid
    , legend.background = element_rect(fill = "transparent") # get rid of legend bg
    , legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
  )

ggsave(paste(plot_directory, "racial_plot.png", sep="/"), racial_plot, width = 8, height = 5)
ggsave(paste(plot_directory, "racial_plot_transparent.png", sep="/"), racial_plot_transparent, width = 8, height = 5, bg = "transparent")
##  ............................................................................
##  Plot racial percentile vs overall percentile - diff linetypes           ####

racial_plot_different_linetypes <- racial_wealth_pctiles %>%
  filter(between(total_ntile, 1, 99)) %>%
  ggplot() +
  geom_smooth(aes(x = as.numeric(race_ntile), y = total_ntile, color = race, linetype = race),
              se=FALSE,
              span = .1) +
  geom_segment(data=data.frame(x=0, y=0, xend = 100, yend=100, race = "Equality"),
               aes(x = x, y = y, xend = xend, yend = yend, color="Equality", linetype = race),
               , show.legend = TRUE) +
  scale_color_colorblind() +
  xlab("Racial Wealth Percentile") +
  ylab("Overall Wealth Percentile") +
  ggtitle("Racial Wealth Percentile by Overall Wealth Percentile (2016)") +
  labs(caption = "Source: Survey of Consumer Finances") +
  theme_ipsum(grid = "Y") +
  theme(legend.title = element_blank()) +
  scale_linetype_manual(values = c("White" = 1,"Latino" = 3, "Black" = 4, "Equality" = 6))

racial_plot_different_linetypes_transparent <-
  racial_plot_different_linetypes + theme(
  panel.background = element_rect(fill = "transparent") # bg of the panel
  , plot.background = element_rect(fill = "transparent") # bg of the plot
  , panel.grid.major = element_blank() # get rid of major grid
  , panel.grid.minor = element_blank() # get rid of minor grid
  , legend.background = element_rect(fill = "transparent") # get rid of legend bg
  , legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
)

ggsave(paste(plot_directory, "racial_plot_linetypes.png", sep="/"), racial_plot_different_linetypes, width = 8, height = 5)
ggsave(paste(plot_directory, "racial_plot_linetypes_transparent.png", sep="/"), racial_plot_different_linetypes, width = 8, height = 5, bg = "transparent")
