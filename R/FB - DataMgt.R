

# Loading/installing required packages

pkgs <- c("tidyverse",
          "waffle",
          "lubridate",
          "tidytext",
          "extrafont",
          "showtext",
          "kableExtra",
          "stargazer",
          "scales",     # for automatically determining breaks and labels for axes and legends.
          "ggpubr",      # Publication Ready Plots
          "ggpol",       # Data Visualization - geom_boxjitter
          "stringr",
          "htmlwidgets",
          "tweenr",
          "webshot",
          "readxl",
          "MASS"
)

# # "mosaic",
# "oddsratio",

miss_pkgs <- pkgs[!pkgs %in% 
                    installed.packages()[,1]] 

# Installing the missing packages
      if(length(miss_pkgs)>0){
        install.packages(miss_pkgs)
      }
      
      # Loading all the packages
      invisible(lapply(pkgs,library,character.only=TRUE))
      rm(miss_pkgs)
      rm(pkgs)
      




## Function for MultiDate
multidate <- function(data, formats){
  a<-list()
  for(i in 1:length(formats)){
    a[[i]]<- as.Date(data,format=formats[i])
    a[[1]][!is.na(a[[i]])]<-a[[i]][!is.na(a[[i]])]
  }
  a[[1]]
}



advancingSexEd <- readxl::read_xlsx("../../Data/FB Data - SRH.xlsx") 
advancingSexEd$post_date <- multidate(advancingSexEd$post_date, 
                                      c("%Y-%m-%d", "%D-%m-%y",
                                        "%d-%m-%y", "%m/%d/%Y"))



advancingSexEd <- advancingSexEd %>% 
                  mutate(postid = as.character(postid)) %>% 
                  filter(is.na(tone) == FALSE) %>% 
                  filter(topic != "Parenting" &
                           topic != "STIs including HIV/AIDs") %>% 
                  mutate(post_type = as.factor(post_type),
                         topic = as.factor(topic),
                         purpose = as.factor(purpose),
                         tone = as.factor(tone)) %>% 
  
                  droplevels() %>% 
                  
                  mutate(
                    month = post_date %>% month(label = TRUE) %>% as.factor(),
                    year = post_date %>% year(),
                    day = post_date %>% day(),
                    week = post_date %>% isoweek(),
                    wday = post_date %>% wday(week_start = 1, label = TRUE))


advancingSexEd <- advancingSexEd %>% 
  dplyr::select(c("post", "post_date", "post_shares",
                   "post_comments", "post_reactions",
                   "post_type", "topic", "purpose",
                   "tone", "wday"))


annotated <- c("purpose", "tone", "topic", "wday", "post_type")
FB_SRH <- read_xlsx("../../Data/FB Group SRH Data.xlsx") %>% 
  mutate_each_(~(factor(.)),annotated)


dictionary <- read.csv("../../Data/dictionary.csv")
codebook <- read.csv("../../Data/codebook.csv")






model_Data <- FB_SRH %>% 
  
              dplyr::select("post_date", "wday", "topic",
                            "purpose", "tone", "post_type",
                            "post_reactions",
                            "post_shares", "post_comments") %>% 
  
              mutate(post_type = replace(post_type, 
                                         which(post_type == "photo" |
                                               post_type == "video"), "photo")) %>% 
              
              mutate(total_inter = post_comments + post_shares + post_reactions) %>% 
              rename(post_style = purpose,
                     post_tone = tone,
                     post_topic = topic) %>% 
  
              droplevels()

write.csv(model_Data, "../../Data/FB-Advancing SRH.csv")

# writexl::write_xlsx(advancingSexEd, "../../Data/FB Group SRH Data.xlsx")