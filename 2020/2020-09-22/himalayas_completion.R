# load libraries
library(tidyverse)
library(scales)
library(extrafont)

# Get the Data
tuesdata <- tidytuesdayR::tt_load('2020-09-22')

# set dataframes
peaks <- tuesdata$peaks
# members <- tuesdata$members
# expeditions <- tuesdata$expeditions

# create a colour palette
col_pal <- c("#FED880", "#AED7E0", "#004680", "#15A3D3", "#FAF3EB")


peaks %>% 
  filter(first_ascent_year != 201) %>% 
  group_by(first_ascent_year) %>% 
  summarise(peaks_climbed = n_distinct(peak_id)) %>% 
  mutate(perc_climbed = peaks_climbed / nrow(peaks)) %>% 
  mutate(cum_perc = cumsum(perc_climbed)) %>% 
  ggplot(aes(x=first_ascent_year, y= cum_perc)) +
  geom_area(fill = col_pal[5]) +
  scale_y_continuous(labels = percent, limits = c(0,1)) +
  geom_hline(yintercept = 1, colour = col_pal[5], linetype=2) +
  scale_x_continuous(limits = c(1900, 2025), labels = seq(1900, 2025, 25), breaks = seq(1900, 2025, 25)) +
  geom_segment(aes(x = 1953, xend = 1982, y = 0.1, yend = .40), colour = col_pal[1],
               arrow = arrow(length = unit(0.3, "cm")), size=0.4) +
  geom_segment(aes(x = 2001, xend = 2018, y = 0.47, yend = .73), colour = col_pal[1],
               arrow = arrow(length = unit(0.3, "cm")), size=0.4) +
  annotate("text", x=1945, y= 0.25, label = "Two distinct periods of new peaks\nbeing ascended between 1953-1984\nand (2001 to current)", 
           colour = col_pal[1], family = "Andale Mono") +
  geom_segment(aes(x = 1984, xend = 2000, y = 0.4, yend = .455), colour = col_pal[2],
               arrow = arrow(length = unit(0.3, "cm")), size=0.4) +
  annotate("text", x=1978, y= 0.48, label = "While there was a declining rate\nof new peaks climbed between the\nmid 1980s to 2001",
           colour = col_pal[2], family = "Andale Mono") +
  labs(x= "Year", y= "Percent of Peaks Climbed",
       caption = "Source: The Himalayan Database  ||  Visualisation: @Jase_Ziv83") +
  ggtitle("STILL A MOUNTAIN TO CLIMB",
          subtitle = paste0("The percentage of peaks in the Himalayas being climbed has\nbeen increasing through the 2000s, though there are still\n",
                            percent(sum(is.na(peaks$first_ascent_year)) / nrow(peaks)), " of the ", nrow(peaks), " peaks remaining to be attempted")) +
  theme_bw() + 
  theme(
    text = element_text(family = "Andale Mono", 
                        color = grey(0.9)),
    title = element_text(family = "Andale Mono", size = 30),
    rect = element_rect(fill = "#101924ff", color = NA),
    panel.background = element_rect(fill = "#101924ff", color = NA),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "#252f39ff"),
    plot.margin = margin(20, 40, 5, 40),
    plot.background = element_rect(color = NA),
    plot.title.position = "panel",
    plot.title = element_text(margin = margin(0,0,0,0)),
    plot.subtitle = element_text(family = "Andale Mono", 
                                 size = 14, margin = margin(0,0,20,0)),
    plot.caption = element_text(family = "Andale Mono", size = 8, color = grey(0.6)),
    axis.title.y = element_text(family = "Andale Mono", size = 12,
                                color = grey(0.9)),
    axis.title.x = element_text(family = "Andale Mono", size = 12,
                                color = grey(0.9)),
    axis.text.x = element_text(color = grey(0.7), size = 10),
    axis.text.y = element_text(color = grey(0.7), size = 10),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank()
  )


ggsave("2020/2020-09-22/himalayas_completed.png", width = 11, height = 10)
