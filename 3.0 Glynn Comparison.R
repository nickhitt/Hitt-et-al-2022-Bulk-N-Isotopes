
### This script produces the Main Text Detrended d15N Figures for Hitt et al 2022


library(dplyr)
library(readxl)
library(stats)
library(gridExtra)
library(ggplot2)
library(ggpubr)
library(zoo)
library(pracma)

source("~/Dropbox/Marsden Black Coral Project/R Codes/Hitt-et-al-2022-Bulk-N-Isotopes/detrend_functions.R")

locations <- c("EAuC", "STF", "SH")

data <- data.frame(read_excel("~/Dropbox/Marsden Black Coral Project/R Codes/Hitt-et-al-2022-Bulk-N-Isotopes/Nitrogen Isotope Data_edit2.xlsx")) %>%
  dplyr::mutate(Coral_name = case_when(Coral == "35104" ~ "EAuC2",
                                       Coral == "64344" ~ "EAuC1",
                                       Coral == "47996" ~ "STF1",
                                       Coral == "SH9" ~ "SH9",
                                       Coral == "SH10" ~ "SH10"),
                location = case_when(grepl(locations[1], Coral_name) ~ "East Auckland Current",
                                     grepl(locations[3], Coral_name) ~ "East Australian Current Extension",
                                     grepl(locations[2], Coral_name) ~ "Subtropical Front")) %>%
  select(-err, -Coral) %>%
  dplyr::filter(Coral_name != "SH10")

#Loading in Glynn Data
glynn_data <- data.frame(read_excel("~/Dropbox/Marsden Black Coral Project/R Codes/Bulk Carbon Isotopes/Glynn et al Data.xlsx")) %>%
  rename(age = Year, Coral_name = ID, d15n = d15N) %>%
  dplyr::filter(d15n != "-") %>%
  select(-d13C) %>%
  mutate(location = "North Pacific",
         d15n = round(as.numeric(d15n), digits = 2)) %>%
  dplyr::filter(grepl("L", Coral_name)) %>%
  dplyr::filter(age <3000)

pacific_data <- data %>%
  rbind(glynn_data) 

means <- aggregate(d15n ~ Coral_name, pacific_data, mean)


figure_1 <- pacific_data %>%
  filter(Coral_name != "STF1") %>%
  filter(Coral_name != "SH9") %>% 
  #filter(age <3000 & age > 1800) %>%
   #mutate(anom = case_when(#Coral_name == "SH9" ~ d15n - means[5,2],
                           #Coral_name == "SH10" ~ d15n - means[4,2],
                           #Coral_name == "L2" ~ d15n - means[4,2],
                           #Coral_name == "EAuC2" ~ as.numeric(d15n) - means[2,2])) %>%
  ggplot(mapping = aes(age, d15n, group = Coral_name)) +
  geom_point(aes(colour=Coral_name)) + 
  #geom_line(aes(colour=Coral_name)) +
  geom_smooth(smethod = "loess",  span = 0.1, aes(colour = Coral_name)) +
  xlab("Time (cal BP)") +
  scale_x_continuous(breaks=seq(0,3000,500)) +
  facet_grid(rows = vars(location),scales = "free_y") +
  theme(panel.background = element_rect(fill = "white", colour = "black", size = 1),
        legend.box.background = element_rect(),
        legend.box.margin = margin(6, 6, 6, 6),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) 
figure_1$labels$colour <- "Coral"
figure_1$labels$y <- expression(paste("Bulk ", "\u03B4" ^ "15", "N ", "(\u2030)"))

figure_1
