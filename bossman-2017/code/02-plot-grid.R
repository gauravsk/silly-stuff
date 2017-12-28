# Plot activities as small multiples

# Load packages
library(ggart) # devtools::install_github("marcusvolz/ggart")
library(tidyverse)

# Read in pre-processed data
data <- readRDS("processed/data.RDS")

# The races were...
races <- c(10, 10, 34:37, 63, 106, 110, 125:127, 132, 144, 153, 164, 183, 189, 207, 226)
marathons <- c(10, 110, 183)
halfathon <- c(226, 207, 144, 132, 153)
fiveks <- c(19, 63, 189, 164)
other_race <- races[which(!(races %in% c(marathons, halfathon, fiveks)))]
data <- data %>% mutate(runtype = ifelse(id %in% races, "Race", "Run")) %>%
  mutate(runtype = ifelse(id %in% marathons, "Marathon", runtype)) %>%
  mutate(runtype = ifelse(id %in% halfathon, "Half Marathon", runtype)) %>%
  mutate(runtype = ifelse(id %in% fiveks, "5K", runtype)) %>%
  mutate(runtype = ifelse(id %in% other_race, "Other Race", runtype)) %>% tbl_df

data <- data %>% mutate(lsize = ifelse(runtype == "Run", 1, 1.001))

data %>% mutate()
summary <- data %>%
  dplyr::group_by(id) %>%
  dplyr::summarise(lon = mean(range(lon)),
                   lat = mean(range(lat)),
                   distance = as.numeric(sprintf("%.1f", max(cumdist)))) %>% 
  mutate(emph = distance/max(distance))
# Create plot
p <- ggplot() +
  geom_path(aes(lon, lat, group = id, size = data$lsize, color = as.factor(data$runtype)), data, lineend = "round") +
  # geom_text(ggplot2::aes(lon, lat, label = distance), data = summary, alpha = 0.25, size = 3)  +
  facet_wrap(~id, scales = "free", ncol = 15) +
  theme_blankcanvas(margin_cm = 1) + ggtitle("Runs of 2017")  + 
  theme(panel.spacing = unit(0, "lines"), legend.position = "bottom", 
        legend.title = element_blank(), legend.text=element_text(size=25), legend.key = element_blank(),
        legend.key.width = unit(3, "cm"),
        plot.title = element_text(hjust = .5, size = 40, margin = margin(b = 1, unit = "cm"))) + 
  scale_colour_manual(values=c("#AA3939","#804515","#116611", "#0D4D4D", "black")) + 
  scale_size(guide = FALSE) +
  guides(colour = guide_legend(override.aes = list(size=4))) 

# Save plot
dir.create("plots")
ggsave("plots/facets003.pdf", p, width = 22, height = 22, units = "in")
ggsave("plots/facets002.png", p, width = 22, height = 22, units = "in")


