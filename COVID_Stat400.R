library(dplyr)
library(ggplot2)
library(tidyverse)
library(janitor)
library(gganimate)
library(gifski)
library(png)
library(ggrepel)
library(scales)
library(janitor)
library(maps)


#change the path below to the address to the file on your computer.
#setwd()
path <- "~/Desktop/Teaching/R_Scripts/Covid_Project/Covid_Stat400/COVID-19-master_Nov3/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"
#alternately use setdir() to set directory to the directory containing this script
df<- read.csv(path)
head(df)

covid_full <- df %>% select(c(7, 12:297)) 

covid_states <- covid_full %>% group_by(Province_State) %>% #adding up all the county-level numbers 
  summarize_at(vars(2:286), sum) %>% adorn_totals('row')
covid_states$Province_State <- tolower(covid_states$Province_State)

#unique(covid_map_full$Province_State)
us_states <- map_data("state")
cont_states <- unique(us_states$region) #contiguous states in US

#get information about contiguous states
covid_cont_states <- covid_states %>% 
  filter(Province_State %in%  cont_states) %>% 
  dplyr::rename(region = Province_State)

col_names<- colnames(covid_cont_states)[c(2:286)]
new_col_names <- seq(as.Date("2020/1/21"), by = "day", length.out = 286) #get dates in date-time format

covid_cont_states <- covid_cont_states %>% 
  setNames(new_col_names) %>%
  dplyr::rename(region = "2020-01-21")

#Prepare data for plotting
covid_cont_state.long <- pivot_longer(covid_cont_states, 
                                      cols = c(2:286), 
                                      names_to = "date", 
                                      values_to = "cases")
  
#Choose states to plot
plot_ny <- c("new york")
plot_states <- c("new york", "florida", "texas", "california", "maryland", "minnesota", "connecticut")

#data for plotting specific states
covid_plot_data <- covid_cont_state.long %>% filter(region == plot_states)
covid_data_ny <- covid_cont_state.long %>% filter(region == plot_ny)
covid_data_md <- covid_cont_state.long %>% filter(region == "maryland")

###############################################
######Plot the data ##########################
###############################################

#This is the basic plot 
plot_static <- covid_plot_data %>% 
  ggplot(aes(x = as.Date(date), 
             y = cases, 
             group = region,
             color = region))+
  labs(title = "Time Series Plot of Confirmed Cases of COVID",
       subtitle = "Jonathan Fernandes PhD, University of Maryland, College Park.",
       x = "Date", 
       y = "Number of Cases")+
  geom_line()+
  scale_x_date(limits = as.Date(c("2020-1-22","2020-11- 02")), 
               date_breaks = "1 month",
               date_labels="%B")+
  scale_y_continuous(labels = scales::comma)+
  theme(
    plot.title = element_text(color="black", size=14, face="bold.italic"),
    axis.title.x = element_text(color="black", size=10),
    axis.title.y = element_text(color="black", size=10))+
  facet_grid(~region)

plot_static


#This plot is to be used when making animations
plot_anim <- covid_plot_data %>% 
  ggplot(aes(x = as.Date(date), 
             y = cases, 
             group = region,
             color = region))+
  labs(title = "Time Series Plot of Confirmed Cases of COVID",
       subtitle = "Jonathan Fernandes PhD, University of Maryland, College Park.",
       x = "Date", 
       y = "Number of Cases")+
  geom_line()+
  geom_point()+
  geom_text(aes(label = factor(region)), 
            hjust = 0, 
            position = position_dodge(width=0.9),  
            size=4)+
  scale_x_date(limits = as.Date(c("2020-1-22","2020-11- 02")), 
               date_breaks = "1 month",
               date_labels="%B")+
  scale_y_continuous(labels = scales::comma)+
  theme(
    plot.title = element_text(color="black", size=14, face="bold.italic"),
    axis.title.x = element_text(color="black", size=10),
    axis.title.y = element_text(color="black", size=10),
    legend.position = "none") #+ facet_grid(~region) use facet grid if you want separate plots

plot_anim 

#to compute the animation
plot_anim <- plot_anim +  theme(
  #panel.background = element_rect(fill = "white"),
  plot.margin = margin(5,10 ,5, 1, "mm"),
  plot.background = element_rect(
    fill = "white",
    colour = "black",
    size = 1
  )
)

plot_anim <- plot_anim +transition_reveal(as.Date(date))
### render animation 
plot_anim <- animate(plot_anim,
                       width = 750, 
                       height = 650,
                       duration = 20,
                       end_pause = 20,
                       renderer = gifski_renderer())

plot_anim


#### Plots for New York ######################
ny <- covid_data_ny %>% 
  ggplot(aes(x = as.Date(date), 
             y = cases, 
             group = region,
             color = region))+
  labs(title = "Time Series Plot of Confirmed Cases of COVID",
       subtitle = "Jonathan Fernandes PhD, University of Maryland, College Park.",
       x = "Date", 
       y = "Number of Cases")+
  geom_line()+
  scale_x_date(limits = as.Date(c("2020-1-22","2020-11- 02")), 
               date_breaks = "1 month",
               date_labels="%B")+
  scale_y_continuous(labels = scales::comma)+
  theme(
    plot.title = element_text(color="black", size=14, face="bold.italic"),
    axis.title.x = element_text(color="black", size=10),
    axis.title.y = element_text(color="black", size=10))

ny

ny <- covid_data_ny %>% 
  ggplot(aes(x = as.Date(date), 
             y = cases, 
             group = region,
             color = region))+
  labs(title = "Time Series Plot of Confirmed Cases of COVID",
       subtitle = "Jonathan Fernandes PhD, University of Maryland, College Park.",
       x = "Date", 
       y = "Number of Cases")+
  geom_line()+
  geom_point()+
  geom_text(aes(label = factor(region)), 
            hjust = 0, 
            position = position_dodge(width=0.9),  
            size=4)+
  scale_x_date(limits = as.Date(c("2020-1-22","2020-11- 02")), 
               date_breaks = "1 month",
               date_labels="%B")+
  scale_y_continuous(labels = scales::comma)+
  theme(
    plot.title = element_text(color="black", size=14, face="bold.italic"),
    axis.title.x = element_text(color="black", size=10),
    axis.title.y = element_text(color="black", size=10),
    legend.position = "none")#+facet_grid(~region)

#########################################################################
########################################################################
###############
########  Analyzing by state, at the county level ##################
################
###########################################################################

covid_counties <- df %>% select(6:7, 12:297) #contains information at county level
covid_counties$Province_State <- tolower(covid_counties$Province_State)

#choose state to analyze 
state_choice <- c("maryland")

covid_state_counties <- covid_counties %>% 
  filter(Province_State %in% state_choice) %>% select(1, 3:288)

new_col_names <- seq(as.Date("2020/1/21"), by = "day", length.out = 287) #get dates in date-time format

covid_state_counties <- covid_state_counties %>% 
  setNames(new_col_names) %>% dplyr::rename(region = "2020-01-21")


#Prepare data for plotting
covid_counties.long <- pivot_longer(covid_state_counties, 
                                      cols = c(2:287), 
                                      names_to = "date", 
                                      values_to = "cases")

#Choose county to plot
plot_county <- c("Baltimore", "Prince George's", "Montgomery")

#data for plotting specific states
covid_plot_data <- covid_counties.long %>% filter(region == plot_county)

###############################################
######Plot the data ##########################
###############################################

#This is the basic plot 
plot_static <- covid_plot_data %>% 
  ggplot(aes(x = as.Date(date), 
             y = cases, 
             group = region,
             color = region))+
  labs(title = "Time Series Plot of Confirmed Cases of COVID",
       subtitle = "Jonathan Fernandes PhD, University of Maryland, College Park.",
       x = "Date", 
       y = "Number of Cases")+
  geom_line()+
  scale_x_date(limits = as.Date(c("2020-1-22","2020-11- 02")), 
               date_breaks = "1 month",
               date_labels="%B")+
  scale_y_continuous(labels = scales::comma)+
  theme(
    plot.title = element_text(color="black", size=14, face="bold.italic"),
    axis.title.x = element_text(color="black", size=10),
    axis.title.y = element_text(color="black", size=10))#+facet_grid(~region)

plot_static




##########################################################################
################ Working with Maps #######################################
#########################################################################


us_states <- map_data("county")
names(us_states)
unique(us_states$subregion)

p <- ggplot(data = us_states,
            mapping = aes(x = long, y = lat,
                          group = group))
library(mapproj)
p + geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  guides(fill = FALSE)

p + geom_polygon(fill = "white", color = "black")



####################Working with Covid data and maps ###############3
library(dplyr)
library(ggplot2)
library(tidyverse)
library(janitor)
library(gganimate)
library(gifski)
library(png)
library(ggrepel)
library(scales)

path <- "~/Desktop/Teaching/R_Scripts/Covid_Project/COVID-19-master_Nov3/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"
df <- read.csv(path)
head(df)

covid_map_full <-  df %>% select(c(7,9:11, 12:297)) #select the required columns
#covid_map_full_alabama <- filter(covid_map_full, Province_State == 'Alabama')

library(janitor)
covid_map_state <- covid_map_full %>% group_by(Province_State) %>% 
  summarize_at(vars(5:289), sum) %>% adorn_totals('row')
covid_map_state$Province_State <- tolower(covid_map_state$Province_State)

#unique(covid_map_full$Province_State)
us_states <- map_data("state")
cont_states <- unique(us_states$region) #contiguous states

covid_cont_states <- covid_map_state %>% 
  filter(Province_State %in%  cont_states) %>%
  dplyr::rename(region = Province_State)

#prepare data for plotting dynamic/animated map. Need to use the longer pivoted table
col_names <- colnames(covid_cont_states)[c(2:186)]
new_col_names <- seq(as.Date("2020/1/21"), by = "day", length.out = 286) #get dates in date-time format

covid_cont_states <- covid_cont_states %>% 
  setNames(new_col_names) %>%
  dplyr::rename(region = "2020-01-21")

#We now have the data in the final form that we want. Now we will pivot along the date columns. 
covid_cont_state.long <- pivot_longer(covid_cont_states, 
                                      cols = c(280), 
                                      names_to = "date", 
                                      values_to = "cases")
#get US states map data
us_states <- map_data("state")

covid_map_anim_data <- right_join(us_states, covid_cont_state.long, by = "region")

p <- ggplot()+
  geom_polygon(data = covid_map_anim_data, 
               aes(x = long, 
                   y = lat, 
                   group = group,
                   fill = cases)) +
  labs(title="Confirmed Cases in US") +
  scale_fill_gradient(low = "white", high = "purple", na.value="grey80")


#anim <- p + transition_manual(date)
#animate(p)


p + geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) 

#p + transition_reveal(date,renderer = gifski_renderer())


################################################################################
#########Create a function that given a index, creates a heat map of confirmed cases for the index corresponding to the date
##################################################################################

pivot_on_column <- function(i, data){
  pivot_longer(data, 
               cols = c(i), 
               names_to = "date", 
               values_to = "cases")
  
}

create_plot <- function(i, data){
  plot_data <- pivot_on_column(i, data)
  us_states <- map_data("state")
  plot_data <- left_join(us_states, plot_data)
  p <- ggplot()+
    geom_polygon(data = plot_data, 
                 aes(x = long, 
                     y = lat, 
                     group = group,
                     fill = cases)) +
    labs(title="Daily Cases in US") +
    scale_fill_gradientn(#low = "sky blue", 
      #high = "red",
      #mid = "white",
      #space = "Lab",
      colours = c("light blue", "red"),
      na.value="grey80")+ 
    geom_polygon(color = "gray90", size = 0.1) +
    coord_map(projection = "albers", lat0 = 39, lat1 = 45)+
    theme(#legend.position = "none",
      axis.title = element_blank(),
      #axis.title.y = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank())
  return(p)
}

create_plot(175, covid_cont_states)
covid_daily <- data.frame(covid_cont_states)

for(i in c(2:175)){
  #covid_daily[,i] <- covid_cont_states[,i] - covid_cont_states[,i-1]
  ggsave(paste(i,".png"), create_plot(i, covid_daily))
}

create_plot(175, covid_daily)

library(gifski)
png_files <- list.files("/home/jon/COVID_Confirmed_plots/", full.names = TRUE)
gifski(mixedsort(png_files), gif_file = "covid_confirmed.gif", width = 800, height = 600, delay = .1)
