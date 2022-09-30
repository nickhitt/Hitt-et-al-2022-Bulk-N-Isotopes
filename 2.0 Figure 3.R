### This script produces the Main Text Detrended d15N Figures for Hitt et al 2022


library(dplyr)
library(readxl)
library(tidyr)
library(broom)
library(stats)
library(gridExtra)
library(ggplot2)
library(ggpubr)
library(zoo)
library(pracma)

source("~/Dropbox/Marsden Black Coral Project/R Codes/Hitt-et-al-2022-Bulk-N-Isotopes/detrend_functions.R")

data <- data.frame(read_excel("~/Dropbox/Marsden Black Coral Project/R Codes/Hitt-et-al-2022-Bulk-N-Isotopes/Nitrogen Isotope Data_edit2.xlsx")) %>%
  dplyr::mutate(Coral_name = case_when(Coral == "35104" ~ "EAuC2",
                                       Coral == "64344" ~ "EAuC1",
                                       Coral == "47996" ~ "STF1",
                                       Coral == "SH9" ~ "SH9",
                                       Coral == "SH10" ~ "SH10"))

means <- aggregate(d15n ~ Coral_name, data, mean)

coral_steps <- rep(0,3)
for (i in 1:3){
  coral <- c("EAuC1", "EAuC2", "STF1", "SH9", "SH10")
  new_df <- data %>%
    filter(Coral_name %in% coral[i]) %>%
    select(age)
  new_age <- rep(0, length(new_df-1))
  for (q in 1:length(new_age)){
    new_age[q] <- new_df$age[q+1] - new_df$age[q]
  }
  coral_steps[i] <- max(new_age)
}

min_age <- ceiling(min(data[which(data$Coral_name == "EAuC1"),1])) 
max_age <- floor(max(data[which(data$Coral_name == "SH9"),1])) 

time_vec <- seq(from = min_age, to = max_age, by = ceiling(max(coral_steps)))
time_vec <- data.frame(time_vec) %>%
  rename(age_int = time_vec)

detrended_eauc1 <- detrend_coral(data, "EAuC1", "d15n") 
detrended_eauc2 <- detrend_coral(data, "EAuC2", "d15n") 
detrended_stf1 <- detrend_coral(data, "STF1", "d15n")
detrended_sh9 <- detrend_coral(data, "SH9", "d15n")
detrended_sh10 <- detrend_coral(data, "SH10", "d15n")

detrended_eauc1_int <- detrend_coral_interp(data, "EAuC1", "d15n", time_vec) 
detrended_eauc2_int <- detrend_coral_interp(data, "EAuC2", "d15n", time_vec) 
detrended_stf1_int <- detrend_coral_interp(data, "STF1", "d15n", time_vec)
detrended_sh9_int <- detrend_coral_interp(data, "SH9", "d15n", time_vec) 
colnames(detrended_sh9_int) <- c("age_int", "d15n", "Coral_name")
detrended_sh10_int <- detrend_coral_interp(data, "SH10", "d15n", time_vec)
colnames(detrended_sh10_int) <- c("age_int", "d15n", "Coral_name")


figure3a <- rbind(detrended_eauc2_int, detrended_eauc1_int) %>%
  ggplot(mapping = aes(age_int, d15n, group = Coral_name)) +
  geom_point(aes(colour = Coral_name)) + 
  geom_line(aes(colour = Coral_name)) +
  xlim(0, 3000) +
  xlab("Time (cal BP)") +
  scale_x_continuous(breaks=seq(0,4500,500)) +
  theme(panel.background = element_rect(fill = "white", colour = "black", size = 1),
        legend.background = element_rect(size = 0.5, colour = 1),
        legend.text = element_text(size=8),
        legend.title = element_text(size=8),
        panel.grid.major = element_blank(), 
        legend.key = element_rect(colour = "transparent", fill = "white"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.direction = c("horizontal"),
        legend.position= c(0.2,0.15)) 

figure3a$labels$colour <- "Coral"
figure3a$labels$y <- expression(paste("Detrended Bulk ", "\u03B4" ^ "15", "N", " (\u2030)"))

figure3a$labels$title <- expression(paste("Interpolated Southwest Pacific Corals Detrended Bulk ", "\u03B4" ^ "15", "N Data"))

figure3a

all_corals <- rbind(detrended_eauc2_int, detrended_eauc1_int, detrended_sh9_int,detrended_sh10_int)

all_corals_reshaped <- all_corals %>%
  pivot_wider(id_cols = age_int, names_from=Coral_name,values_from=d15n) %>%
  arrange(age_int)

figure3b <- all_corals_reshaped %>%
  select(EAuC1, EAuC2,age_int) %>%
  filter(!is.na(EAuC2) | !is.na(EAuC1)) %>%
  ggplot(mapping = aes(EAuC1, EAuC2)) +
  geom_point() + 
  geom_smooth(method="lm") +
  theme(panel.background = element_rect(fill = "white", colour = "black", size = 1),
        legend.box.background = element_rect(),
        legend.box.margin = margin(6, 6, 6, 6),
        panel.grid.major = element_blank(), 
        legend.key = element_rect(colour = "transparent", fill = "white"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5)) 
figure3b$labels$x <- expression(paste("EAuC1 Bulk ", "\u03B4" ^ "15", "N", " (\u2030)"))
figure3b$labels$y <- expression(paste("EAuC2 Bulk ", "\u03B4" ^ "15", "N", " (\u2030)"))

figure3b$labels$title <- expression(paste("Southwest Pacific Corals Detrended Bulk ", "\u03B4" ^ "15", "N Regression"))

figure3b

figure3 <- ggarrange(figure3a, figure3b,  
                     labels = c("A", "B"),
                     ncol = 1, nrow = 2)

figure3

### Correlation Analysis

all_corals <- rbind(detrended_eauc2_int, detrended_eauc1_int, detrended_sh9_int,detrended_sh10_int)

all_corals_reshaped <- all_corals %>%
  pivot_wider(id_cols = age_int, names_from=Coral_name,values_from=d15n) %>%
  arrange(age_int)

eauc1_corr <- all_corals_reshaped %>%
  filter(age_int < 1500 & age_int > 600) %>%
  summarise(
    eauc1_eauc2 = cor(EAuC2, EAuC1),
    eauc1_sh9 = cor(EAuC1,SH9),
    eauc2_sh9 = cor(EAuC2,SH9)
  )

eauc2_corr <- all_corals_reshaped %>%
  filter(age_int > 600) %>%
  summarise(
    eauc2_sh9 = cor(EAuC2,SH9)
  )

all_corals_reshaped %>%
  filter(age_int > 600) %>%
  do(tidy(lm(EAuC2 ~ SH9, .)))

all_corals_reshaped %>%
  filter(age_int < 1500 & age_int > 600) %>%
  do(tidy(lm(EAuC1 ~ EAuC2, .)))

