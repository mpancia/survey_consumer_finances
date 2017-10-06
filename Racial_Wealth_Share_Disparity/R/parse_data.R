##%######################################################%##
#                                                          #
####                   Parse SCF Data                   ####
#                                                          #
##%######################################################%##
library(tidyverse)
library(here)
library(mitools)
library(srvyr)
library(survey)
library(magrittr)
data_directory <- paste(dirname(here()), "data", sep = "/")
write_data_directory <- here("data")
years <- c(1989, 1992, 1995, 1998, 2001, 2004, 2007, 2010, 2013, 2016)

get_filtered_design <- function (year) {
  base_name <- paste("scf", year)
  main_filename <- paste(data_directory, paste0(base_name, ".rds"), sep = "/")
  replicate_filename <- paste(data_directory, paste0(base_name, " rw.rds"), sep = "/")
  main_data <- readRDS(main_filename)
  replicate_data <- readRDS(replicate_filename)
  filtered_data <-
    main_data %>%
    lapply(function(x) x %>%
             select(wgt, y1, yy1, networth, race, agecl) %>%
             mutate(year = year)
           )

  main_design <-
    svrepdesign(
      weights          = ~wgt ,
      repweights       = replicate_data[ , -1 ] ,
      data             = imputationList( filtered_data ) ,
      scale            = 1 ,
      rscales          = rep( 1 / 998 , 999 ) ,
      mse              = TRUE ,
      type             = "other" ,
      combined.weights = TRUE,
    ) %>%
    subset(agecl == 1)

  main_design
}

if (!file.exists("./data/filtered_designs.rds")){
  filtered_designs <- lapply(years, get_filtered_design)
  filtered_designs %<>% lapply(function(design) design %>%
                                 update(race = as.factor(race)) %>%
                                 update(race = forcats::fct_recode(race, White = "1", Black = "2", Latino = "3", Other = "5")))
  saveRDS(object = filtered_designs, file = "./data/filtered_designs.rds")
} else {
  filtered_designs <- readRDS("./data/filtered_designs.rds")
}

quantile_vector <- seq(0, 1, .01)

calculate_racial_ntiles <- function(design, quantile_vector){
  racial_wealth_pctiles <- lodown:::scf_MIcombine(
    with( design ,
          svyby(
            ~ networth , ~ as.factor(race)  , svyquantile , quantile_vector, se=FALSE
          ) ) )
  racial_wealth_pctiles %<>%
    .$coefficients %>%
    data.frame(variable = names(.), racial_ntile_value = .) %>%
    tidyr::separate(variable, c("ntile_race", "race_ntile_value"), sep = ":V")

  racial_wealth_pctiles
}

calculate_total_ntiles <- function(design, quantile_vector){
  total_wealth_pctiles <- quantile_vector %>%
    lapply(function(x){lodown:::scf_MIcombine(
      with( design , svyquantile( ~ networth , x ) )
    )}
    ) %>%
    lapply(function(x) x$coefficients) %>%
    unlist %>%
    data.frame(total_ntile = as.integer(quantile_vector * 100), total_ntile_value = .)
}

get_ntiles_for_race <- function(race, racial_ntiles){
  racial_ntiles %>%
    filter(ntile_race == race)
}

find_ntile <- function(race, networth, racial_ntiles) {
  race_ntiles <- get_ntiles_for_race(race, racial_ntiles)
  findInterval(networth, race_ntiles$racial_ntile_value)
}

find_total_ntile <- function(networth, ntiles) {
  findInterval(networth, ntiles$total_ntile_value)
}

get_shares_by_year <- function(design, year){
  rn1 <- calculate_racial_ntiles(design, quantile_vector)
  calced_ntiles <- design %>%
    update(racial_ntile = mapply(function(x, y) find_ntile(x, y, rn1), race, networth))
  top_1 <-
    lodown:::scf_MIcombine(
      with(calced_ntiles %>% subset(as.numeric(racial_ntile) >= 99 & as.numeric(racial_ntile) <= 100 & race != "Other"),
           svyby(
             ~ networth , ~ race, svytotal, se = FALSE, estimate.only = TRUE
           )
    )) %>%
    .$coefficients %>%
    data.frame(race = names(.), val = .) %>%
    mutate(type = "top_1")

  bottom_10 <-
    lodown:::scf_MIcombine(
      with(calced_ntiles %>% subset(racial_ntile <= 10 & as.numeric(racial_ntile) <= 100 & race != "Other"),
           svyby(
             ~ networth , ~ race, svytotal, se = FALSE, estimate.only = TRUE
           )
      )) %>%
    .$coefficients %>%
    data.frame(race = names(.), val = .) %>%
    mutate(type = "bottom_10")

  overall <-
    lodown:::scf_MIcombine(
      with(calced_ntiles %>% subset(as.numeric(racial_ntile) <= 100 & race != "Other" ),
           svyby(
             ~ networth , ~ race, svytotal, se = FALSE, estimate.only = TRUE
           )
      )) %>%
    .$coefficients %>%
    data.frame(race = names(.), val = .) %>%
    mutate(type = "overall")

  race_df <- bind_rows(top_1, bottom_10, overall) %>%
    spread(key = type, val = val) %>%
    mutate(top_1_share = 100*top_1 / overall, bottom_10_share = 100*bottom_10 /overall) %>%
    mutate(year = year)

  total_quantiles <- calculate_total_ntiles(design, quantile_vector)
  total_calced_ntiles <- design %>%
    update(total_ntile = sapply(networth, function(x) find_total_ntile(x, total_quantiles)))

  top_1_total <-
    lodown:::scf_MIcombine(
      with(total_calced_ntiles %>% subset(as.numeric(total_ntile) == 100),
           svytotal(~ networth))
      ) %>%
    .$coefficients %>%
    data.frame(race = names(.), val = .) %>%
    mutate(type = "top_1")

  bottom_10_total <-
    lodown:::scf_MIcombine(
      with(total_calced_ntiles %>% subset(as.numeric(total_ntile) <= 10),
           svytotal(~ networth))
    ) %>%
    .$coefficients %>%
    data.frame(race = names(.), val = .) %>%
    mutate(type = "bottom_10")

  overall_total <-
    lodown:::scf_MIcombine(
      with(total_calced_ntiles, svytotal(~ networth))
    ) %>%
    .$coefficients %>%
    data.frame(race = names(.), val = .) %>%
    mutate(type = "overall")

  total_df <- bind_rows(top_1_total, bottom_10_total, overall_total) %>%
    spread(key = type, val = val) %>%
    mutate(top_1_share = 100*top_1 / overall, bottom_10_share = 100*bottom_10 /overall) %>%
    mutate(year = year)

  list(total_df, race_df)
}

plot_data <- mapply(function(design, year) get_shares_by_year(design, year), filtered_designs, years)
total_plot_data <- bind_rows(plot_data[seq(1,length(plot_data), 2)]) %>%
  select(year, bottom_10_share, top_1_share) %>%
  gather("type", "value", -year) %>%
  mutate(type = forcats::fct_recode(type, `Bottom 10%` = "bottom_10_share", `Top 1%` = "top_1_share"))
racial_plot_data <- bind_rows(plot_data[seq(1,length(plot_data), 2) - 1])  %>%
  select(year, bottom_10_share, top_1_share,race) %>%
  gather("type", "value", - year, -race) %>%
  mutate(type = forcats::fct_recode(type, `Bottom 10%` = "bottom_10_share", `Top 1%` = "top_1_share")) %>%
  mutate(racetype = paste0(race, type))

write_csv(racial_plot_data, "./data/racial_plot_data.csv")
write_csv(total_plot_data, "./data/total_plot_data.csv" )





