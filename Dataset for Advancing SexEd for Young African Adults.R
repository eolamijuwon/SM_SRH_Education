

##Title   :   Data Visualization for Advancing Sexual Health Education for Young African Adults
#Author   :   Emmanuel Olamijuwon
#Date     :   September 5, 2019
#Purpose  :   Data Visualization


library(tidyverse)
library(extrafont)
library(showtext)
library(lubridate)



showtext_auto()
font_add_google("Montserrat", "Montserrat")
font_add("Montserrat", "Montserrat-Regular.ttf")
Montserrat <- "Montserrat"


## Theme Modifications
########
        theme_DiB <- 
          
          theme_bw() + 
          
          theme(
            
            # Legend Settings
            legend.title = element_blank(),
            legend.text = element_text(size = 30, face = "bold", family = Montserrat),
            legend.key = element_blank(),
            legend.background = element_blank(),
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.box = "vertical",
            
            # Backgrounds
            strip.background = element_rect(fill = "black"),
            strip.text = element_text(size = 35, face = "bold", 
                                      family = Montserrat, colour  = "white"),
            plot.background = element_blank(),
            plot.margin = margin(0.5,0.5,1.0,1.0, unit = "cm"),
            
            # Axis & Titles
            axis.line = element_blank(),
            axis.title.y = element_text(size = 28, face = "bold",
                                        family = Montserrat, 
                                        angle=90, vjust=2.5),
            axis.text.y=element_text(size = 28, face = "bold",
                                     family = Montserrat),
            
            axis.title.x = element_text(size = 28, face = "bold",
                                        family = Montserrat,
                                        vjust = -2.5),
            axis.text.x = element_blank(),
            
            # Panel
            panel.grid = element_line(colour = NULL),
            panel.grid.major.y = element_line(colour = "#D2D2D2",
                                              linetype = "dashed",
                                              size = 0.3),
            panel.grid.major.x = element_line(colour = "#D2D2D2",
                                              linetype = "dashed",
                                              size = 0.3),
            panel.grid.minor = element_blank(),
            panel.background = element_blank())

############


##==============
advancing_SexEd <- read.csv("./data/advancing SexEd in Africa.csv")
##==============





######=======================================Data Management===========

advancing_SexEd$post_date <- as.Date(advancing_SexEd$post_date)


advancing_SexEd <- advancing_SexEd %>% 
  mutate(engagements = post_reactions + 
           post_comments + 
           post_shares) %>% 
  # select(c("post_date", "engagements")) %>% 
  # as_date(post_date)
  group_by(post_date) %>% 
  mutate(cases = n()) %>% 
  summarize(total_engagement = sum(engagements),
            posts = mean(cases),
            comments = sum(post_comments),
            reactions = sum(post_reactions),
            shares = sum(post_shares),
  ) %>% 
  ungroup() %>% 
  mutate(
    month = post_date %>% month(label = TRUE) %>% as.factor(),
    year = post_date %>% year(),
    day = post_date %>% day(),
    week = post_date %>% isoweek(),
    wday = post_date %>% wday(week_start = 1, label = TRUE)) %>% 
  mutate(average_eng = total_engagement/posts) %>% 
  
  mutate(week = replace(week, which(month == "Dec" & week == 1), 53))

######================================================================




##======= Counts of Facebook Posts
#########=========================

advancing_SexEd %>% 
  
  ggplot(aes(x = post_date)) + 
  geom_step(aes(y = posts), color = "#1B5B83", size = 0.7) +
  
  scale_x_date(date_breaks = "1 month", 
               date_minor_breaks = "1 week",
               date_labels = "%B") +
  labs(x = "Month of the Year (2018 - 2019)", y = "Count of Messages",
       color = "Channel") +
  
  theme_DiB +
  
  theme(axis.text.x = element_text(size = 42),
        axis.text.y = element_text(size = 42),
        axis.title.x = element_text(size = 47),
        axis.title.y = element_text(size = 47),
        plot.title = element_blank(),
        plot.subtitle = element_text(size = 50,
                                     margin = unit(c(0, 0, 0.5, 0), "cm")),
        text = element_text(size = 42),
        legend.text = element_text(size = 42),
        legend.title = element_text(size = 42),
        legend.background = NULL,
        plot.margin=unit(c(1,1,1.2,1.2),"cm"))


ggsave("./figures/Post Count.png", width = 15, height = 8.5, dpi = 300)

#########==============================================================


##======= Counts of Post Enagagments
#########===========================

advancing_SexEd %>% 
  
      ggplot(aes(week, reorder(wday, desc(wday)),
                 fill = average_eng)) +
      geom_tile()+
      
      scale_fill_viridis_c("Messages-Reactions Ratio", option = "C",
                           breaks = c(0, 200, 600, 1000),
                           guide = guide_colorbar(direction = "horizontal",
                                                  title.position = "top")) +
      
      scale_x_continuous(breaks = c(1:4, 10, 20,
                                    30, 40, 50, 52))+
      
      
      facet_grid(year~month, scales = "free") +
      
      guides(colour = guide_colorbar(direction = "horizontal",
                                     title.position = "top")) +
      labs(x = "Week of the Year", y = "Day of the Week") +
      theme_DiB +
      theme(panel.grid = element_blank(),
            panel.border = element_blank(),
            
            axis.text.x = element_text(size = 35),
            axis.title.x = element_text(size = 47),
            
            axis.title.y = element_text(size = 47),
            axis.text.y = element_text(size = 35),
            
            plot.title = element_blank(),
            text = element_text(size = 42),
            legend.position = c(.70, .25),
            legend.title = element_text(),
            legend.key.size = unit(1.0, "cm"),
            legend.key.width = unit(2.0,"cm"),
            legend.box.background = element_rect(fill = "white",
                                                 color = "white"),
            
            plot.margin=unit(c(1,1,1.2,1.2),"cm"))


ggsave(".\\figures\\Level of Engagement.png", width = 15, height = 8, dpi = 300)
#########==============================================================
