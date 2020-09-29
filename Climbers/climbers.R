library(tidyverse)
library(ggplot2)
library(extrafont)
library(ggimage)
library(gganimate)
loadfonts()

# reading data

members <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/members.csv')
expeditions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/expeditions.csv')
peaks <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/peaks.csv')


# counts the number of expeditions

plotdata <- expeditions %>%
  group_by(year) %>%
  count() %>%
  filter(!is.na(n))

# added the image path

plotdata <- plotdata %>%
  mutate(image = "hike.png")

#visualisation

plot <- plotdata %>%
  ggplot() +
  geom_line(aes(y = n, x = year)) +
  geom_area(aes(y = n, x = year), fill = "lightblue", alpha = 0.5) +
  scale_x_continuous(breaks = seq(1900, 2020, by = 10)) +
  geom_image(aes(image = image, x = year, y = n), size = 0.05) + #ggimage package
  
  theme(
    panel.background = element_rect(fill = "#f1f2f6",
                                    color = "black", size = 1),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  ) +
  theme(legend.position = "none") +
  ggtitle("Himalayan Expeditions over the years") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("\nNumber of Expeditions") +
  
  xlab("Year\n") +
  theme(text = element_text(family = "Bahnschrift"))

# used transition_reveal() from gganimate package

plot <- plot + transition_reveal(year) +
  ease_aes('linear', interval = 0.0005)
animate(plot, nframes = 150, fps = 66)

# animation saved 

anim_save("himalayan.gif", animation = last_animation())
