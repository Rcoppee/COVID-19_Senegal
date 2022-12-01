#setwd("C:/Users/Romain/Desktop/senegal")

#Import R packages
library(ggplot2)
library(cowplot)

#import epidemio data
my_data = readr::read_tsv("covid_africa_data.csv")
my_data_deaths = readr::read_tsv("deaths_africa.csv")

#produce a subdata for senegal for the 2020-2021 period
my_senegal = my_data[my_data$location=="Senegal",]
my_senegal=my_senegal[my_senegal$date<"2022-01-01",]
my_senegal=my_senegal[my_senegal$date>="2020-03-02",]

#produce a subdata of deaths in 2020-2021 in Africa
my_data_deaths=my_data_deaths[my_data_deaths$date<"2022-01-01",]
my_data_deaths=my_data_deaths[my_data_deaths$date>="2020-03-02",]

#plot the number of cases and deaths related to COVUD-19 in 2020 and 2021 in Senegal
p1=ggplot(my_senegal) +
  geom_line(aes(x=date, y=new_cases_smoothed*10),size=2) +
  geom_line(aes(x=date, y=new_deaths*10),size=2, color="red") +
  scale_y_continuous(limits = c(0,10000), expand = c(0, 10))+
  scale_x_date(limits = as.Date(c("2020-01-01","2022-01-01")))+
  theme(axis.line.x = element_line(size = 1, colour = "black"),
        axis.line.y = element_line(size = 1, colour = "black"),
        axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        text=element_text(size = 16, family="Arial"),
        axis.text.x=element_text(colour="black", size = 16),
        axis.text.y=element_text(colour="black", size = 16),
        legend.position = "none")

#plot the number of cases and deaths related to COVID-19 in 2020 and 2021 in Africa
p2= ggplot(my_data_deaths) +
  geom_line(aes(x=date, y=new_cases_smoothed),size=2) +
  geom_line(aes(x=date, y=new_deaths),size=2, color="red") +
  scale_y_continuous(limits = c(0,50000), expand = c(0, 10))+
  scale_x_date(limits = as.Date(c("2020-01-01","2022-01-01")))+
  theme(axis.line.x = element_line(size = 1, colour = "black"),
        axis.line.y = element_line(size = 1, colour = "black"),
        axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        text=element_text(size = 16, family="Arial"),
        axis.text.x=element_text(colour="black", size = 16),
        axis.text.y=element_text(colour="black", size = 16),
        legend.position = "none")

#combine the data based on the date
combined_date = merge(my_data_deaths, my_senegal, by="date")
#calculate the proportion of cases and deaths in Senegal compared to Africa
combined_date$ratio=combined_date$new_cases_smoothed.y/combined_date$new_cases_smoothed.x*100
combined_date$ratio_death=combined_date$new_deaths.y/combined_date$new_deaths.x*100

#plot the proportions of Senegal/Africa in 2020 and 2021
p3=ggplot(combined_date) +
  geom_line(aes(x=date, y=ratio),size=2) +
  #geom_line(aes(x=date, y=new_cases),size=2) +
  geom_line(aes(x=date, y=ratio_death),size=2, color="red", alpha=.5) +
  scale_y_continuous()+
  scale_x_date(limits = as.Date(c("2020-01-01","2022-01-01")))+
  theme(axis.line.x = element_line(size = 1, colour = "black"),
        axis.line.y = element_line(size = 1, colour = "black"),
        axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        text=element_text(size = 16, family="Arial"),
        axis.text.x=element_text(colour="black", size = 16),
        axis.text.y=element_text(colour="black", size = 16),
        legend.position = "none")

#combine the different plots (Africa, Senegal and ratios)
plot_grid(p2, p1,p3, labels=c("A", "B","C"), ncol = 1, nrow = 3)
