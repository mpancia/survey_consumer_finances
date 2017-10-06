library(tidyverse)
library(ggplot2)
library(hrbrthemes)
library(ggthemes)
total_plot_data <- read_csv("./data/total_plot_data.csv")
racial_plot_data <- read_csv("./data/racial_plot_data.csv")

total_vert_lines <- total_plot_data %>%
  group_by(year) %>%
  summarize(y = min(value), ymax = max(value))

racial_vert_lines <- racial_plot_data %>%
  group_by(year, race) %>%
  summarize(y = min(value), ymax = max(value))

years <- total_plot_data$year %>% unique

total_plot_data %>%
  ggplot(aes(x=year)) +
  geom_line(aes(y=value, color = type)) +
  geom_point(aes(y=value, color=type)) +
  geom_label(aes(y=value, label = paste0(round(value), "%"), fill = type, vjust = ifelse(value < 10, 1.2, -.18)), color = "white") +
  geom_segment(data = total_vert_lines, aes(xend = year, y = y, yend = ymax)) +
  geom_label(data = total_vert_lines, aes(y = (ymax - y) / 2, label = paste0(round(ymax - y), "%"))) +
  scale_y_continuous(breaks = seq(-20, 60, 10)) +
  scale_x_continuous(breaks = years) +
  theme_ipsum(grid = FALSE) +
  scale_color_colorblind() +
  scale_fill_colorblind() +
  ylab("Percentage of Wealth Owned") +
  xlab("Year") +
  theme(legend.title = element_blank()) +
  ggtitle("Wealth Share of Top 1% vs. Bottom 10%")


  racial_plot_data %>%
  ggplot(aes(x=year)) +
  geom_line(aes(y=value, group=racetype, color = race)) +
  geom_label(aes(y=value, label = paste0(round(value), "%"), fill = race, vjust = ifelse(value < 10, 1.2, -.18)), color = "white") +
  geom_point(aes(y=value, color=race)) +
  geom_segment(data = racial_vert_lines, aes(xend = year, y = y, yend = ymax, color = race)) +
  geom_label(data = racial_vert_lines, aes(y = (ymax - y) / 2, label = paste0(round(ymax - y), "%"))) +
  facet_wrap(~ race, nrow = 3, ncol = 1) +
  theme_ipsum(grid = FALSE) +
  scale_color_colorblind() +
  scale_fill_colorblind() +
  scale_x_continuous(breaks = years) +
  ylab("Percentage of Wealth Owned") +
  xlab("Year") +
  theme(legend.title = element_blank()) +
  ggtitle("Wealth Share of Top 1% vs. Bottom 10%")
