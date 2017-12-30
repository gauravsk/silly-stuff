# Plot activities as small multiples

# Load packages
library(ggart) # devtools::install_github("marcusvolz/ggart")
library(tidyverse)

# Read in pre-processed data
data <- readRDS("processed/data2.RDS")

# The races were...
races <- c(10, 10, 34:37, 47, 62, 106, 109, 121:123, 128, 149, 153, 164, 183, 189, 207, 226)
marathons <- c(10, 109, 183)
halfathon <- c(226, 207, 149, 128, 153)
fiveks <- c(19, 47, 62, 189, 164)
other_race <- races[which(!(races %in% c(marathons, halfathon, fiveks)))]
data <- data %>% mutate(runtype = ifelse(id %in% races, "Race", "Run")) %>%
  mutate(runtype = ifelse(id %in% marathons, "Marathon", runtype)) %>%
  mutate(runtype = ifelse(id %in% halfathon, "Half Marathon", runtype)) %>%
  mutate(runtype = ifelse(id %in% fiveks, "5K", runtype)) %>%
  mutate(runtype = ifelse(id %in% other_race, "Other Race", runtype)) %>% tbl_df

data <- data %>% mutate(lsize = ifelse(runtype == "Run", 1, 2))

summary <- data %>%
  dplyr::group_by(id) %>%
  dplyr::summarise(distance = as.numeric(sprintf("%.1f", max(cumdist)))) %>% 
  mutate(emph = (distance/max(distance))) 

data <- left_join(data, summary, by = "id")
data <- data %>% mutate(emph = ifelse(runtype != "Run", 1, emph))

# Exclude one loop for now - makes the grid nicer
data <- data %>% filter(id != 84)
# Create plot
# p <- ggplot(data) +
#   geom_path(aes(lon, lat, group = id, alpha = emph, size = emph, color = as.factor(data$runtype)), lineend = "round") +
#   facet_wrap(~id, scales = "free", ncol = 15) +
#   theme_blankcanvas(margin_cm = 1) + ggtitle("Runs of 2017")  + 
#   theme(panel.spacing = unit(1.5, "lines"), legend.position = "bottom", 
#         strip.background=element_rect(linetype = 1),
#         legend.title = element_blank(), legend.text=element_text(size=25), 
#         legend.key = element_blank(), legend.key.width = unit(3, "cm"),
#         plot.title = element_text(hjust = .5, size = 40, margin = margin(b = 1, unit = "cm"))) + 
#   scale_colour_manual(values=c("#AA3939","#804515","#116611", "#0D4D4D", "black")) + 
#   scale_size(guide = FALSE, range = c(.25,2.5)) +
#   scale_alpha(guide = FALSE, range = c(.75, 1)) +
#   guides(colour = guide_legend(override.aes = list(size=4))) 




# Trying to make it different.... ---------
height_per_row <- 24/17

# Set custom theme
theme_stravaplot <- function() {
  theme(panel.spacing = unit(1.5, "lines"), legend.position = "bottom", 
        strip.background=element_rect(linetype = 1),
        legend.title = element_blank(), legend.text=element_text(size=25), 
        legend.margin=margin(5,0,0,0), legend.box.margin=margin(40,0,0,0),
        legend.key = element_blank(), legend.key.width = unit(3, "cm"),
        plot.title = element_text(hjust = .5, size = 40, margin = margin(b = 1, unit = "cm")),
        plot.margin=unit(c(1,1,1,1),"cm"), plot.caption = element_text(face = "bold.italic", size = 25))
}


# The early part of 2017, in Houston. Runs 1-68.
p1 <-  ggplot(data %>% filter(id < 69)) +
  geom_path(aes(lon, lat, group = id, size = emph, color = runtype), lineend = "round") +
  facet_wrap(~id, scales = "free", ncol = 15) +
  theme_blankcanvas(margin_cm = 1) + theme_stravaplot() +
  scale_colour_manual(values=c("#AA3939","#00741F", "#FF7D01", "black"), guide = F) + 
  scale_size(guide = FALSE, range = c(.25,2.5), limits = c(0,1)) + xlab("test") 
ggsave("plots/part1.png", p1, width = 22, height = height_per_row*5, units = 'in')

# Lots of runs during the drive from Texas to Washington! Runs 69-77
p2 <- ggplot(data %>% filter(id %in% 69:83 )) +
  geom_path(aes(lon, lat, group = id,  size = emph, color = runtype), 
            data = data %>% filter(id %in% 69:77), lineend = "round") +
  facet_wrap(~id, scales = "free", ncol = 15, drop = FALSE) +
  theme_blankcanvas(margin_cm = 1) + theme_stravaplot() + 
  scale_colour_manual(values=c("black"), guide = F) + 
  scale_size(guide = FALSE, range = c(.25,2.5), limits = c(0,1)) + xlab("test") 
ggsave("plots/part2.png",p2, width = 22, height = height_per_row*1, units = 'in')

# Running in the Pacific Northwest. Lots of hikes happened during this time that didn't make it on this grid!
# Runs 78 onwards
p3 <- ggplot(data %>% filter(id>77 )) +
  geom_path(aes(lon, lat, group = id,  size = emph, color = runtype), lineend = "round") +
  facet_wrap(~id, scales = "free", ncol = 15, drop = FALSE) +
  theme_blankcanvas(margin_cm = 1) + labs(caption = "2017") +
  theme_stravaplot() + 
  scale_colour_manual(values=c("#AA3939","#04859D","#00741F", "#FF7D01", "black"),
                      breaks = c ("Marathon","Half Marathon", "5K" , "Other Race")) + 
  scale_size(guide = FALSE, range = c(.25,2.5), limits = c(0,1)) + 
  guides(colour = guide_legend(override.aes = list(size=4)))
ggsave("plots/part3.png",p3, width = 22, height = height_per_row*12, units = 'in')


# Now, use a system command to combine all the parts into a single image
system("convert -append plots/part1.png plots/part2.png plots/part3.png plots/combined.png")

