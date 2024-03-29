#### Figure 1 Bulk d15N Paper

library(dplyr)
library(ggplot2)
library(readxl)
library(stats)
library(RColorBrewer)
library(broom)

## Data Import

data <- data.frame(read_excel("~/Dropbox/Marsden Black Coral Project/R Codes/Hitt-et-al-2022-Bulk-N-Isotopes/Nitrogen Isotope Data_edit2.xlsx")) 

############# Data Prep

locations <- c("EAuC", "STF", "SH")

data <- data %>%
  dplyr::mutate(Coral_name = case_when(Coral == "35104" ~ "EAuC 2",
                                       Coral == "64344" ~ "EAuC 1",
                                       Coral == "47996" ~ "STF 1",
                                       Coral == "SH9" ~ "SH9",
                                       Coral == "SH10" ~ "SH10"),
                location = case_when(grepl(locations[1], Coral_name) ~ "East Auckland Current",
                                     grepl(locations[3], Coral_name) ~ "East Australian Current Extension",
                                     grepl(locations[2], Coral_name) ~ "Subtropical Front")) %>%
  select(-err)



means <- aggregate(d15n ~ Coral, data, mean)

## Figure 1

figure_1 <- data %>%
  # mutate(anom = case_when(Coral_name == "SH9" ~ d15n - means[5,2],
  #                         Coral_name == "SH10" ~ d15n - means[4,2],
  #                         Coral_name == "EAuC 1" ~ d15n - means[1,2],
  #                         Coral_name == "EAuC 2" ~ d15n - means[3,2])) %>%
  ggplot(mapping = aes(age, d15n, group = Coral_name)) +
  geom_point(aes(colour=Coral_name)) + 
  geom_line(aes(colour=Coral_name)) +
  xlab("Time (cal BP)") +
  scale_x_continuous(breaks=seq(0,4500,500)) +
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
