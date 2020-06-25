################################################################################
#
## Advancing sexual health education for young African adults in the digital age: 
## Uncovering strategies for successful organic engagement
#
# Data Visualizations
#
################################################################################


## Read Datasets from GitHub              
FB_SRH <- read.csv("./Data/FB_SRH.csv")
codebook <- read.csv("./Data/codebook.csv")



interactions <- data.frame(rbind(cbind(FB_SRH$post_reactions, "Post Reactions"),
                                 cbind(FB_SRH$post_shares, "Post Shares"),
                                 cbind(FB_SRH$post_comments, "Post Comments"))
                           ) %>% 
                rename(count = X1, category = X2) %>% 
                mutate(count = as.numeric(as.character(count)))


## Analysis excludes about two posts with interaction counts more than 1500
filter(interactions,  count< 1500) %>% 
                ggplot(aes(x = factor(category, 
                                      labels = c("Post\nComments",
                                                 "Post\nReactions",
                                                 "Post\nShares")),
                           y = count, 
                           fill=category)) +
                geom_boxjitter(color="black",
                               jitter.color = "darkgrey",
                               errorbar.draw = TRUE) +
                scale_y_continuous() +
                labs(x = "", y = "",
                     caption = "Plot excludes four posts with more than 1,500 
                         \nreactions (2,818, 1,724, 1,691) and shares (3,012)") +
                theme_set +
                # coord_flip() +
                theme(axis.title.y = element_text(size = 26),
                      axis.text.x = element_text(size = 26,
                                                 lineheight = unit(0.35, "pt"),
                                                 margin = unit(c(0, 0.0, 0, 0.5),
                                                               "cm")),
                      axis.title.x = element_text(size = 26),
                      strip.text.x = element_blank(),
                      plot.title = element_text(size = 45),
                      plot.subtitle = element_text(size = 35),
                      legend.position = "none",
                      plot.caption = element_text(size = 22,
                                                  family = Font,
                                                  lineheight = unit(0.25, "pt"),
                                                  face = "italic"))
              
ggsave("./output/Social Interactions.png", dpi = 300, height = 6.5, width = 4)



style_data <- table(FB_SRH$post_topic, FB_SRH$post_style) %>% 
                data.frame() %>% 
                rename (post_topic = Var1,
                        post_style = Var2,
                        Freq_style = Freq) %>% 
                dplyr::group_by(post_topic) %>% 
                mutate(perc_style = round(((Freq_style/sum(Freq_style))*100), digits = 1)) %>% 
                mutate(perc_style = round(perc_style)) %>% 
                mutate(perc_style = replace(perc_style, which(post_topic == "Intimate Relations" &
                                                                    post_style == "Counsel"), 75))


tone_data <- table(FB_SRH$post_topic, FB_SRH$post_tone) %>% 
              data.frame() %>% 
              rename (post_topic = Var1,
                      post_tone = Var2,
                      Freq_tone = Freq) %>% 
              dplyr::group_by(post_topic) %>% 
              mutate(perc_tone = round((Freq_tone/sum(Freq_tone))*100, digits = 2)) %>% 
              mutate(perc_tone = round(perc_tone)) %>% 	
              mutate(perc_tone = replace(perc_tone, which(post_topic == "Abuse" &
                                                            post_tone == "Enact fear"), 1)) %>% 
              mutate(perc_tone = replace(perc_tone, which(post_topic == "Family Planning" &
                                                            post_tone == "Neutral"), 48)) %>% 
              mutate(perc_tone = replace(perc_tone, which(post_topic == "Intimate Relations" &
                                                            post_tone == "Enact fear"), 2)) %>% 
              mutate(perc_tone = replace(perc_tone, which(post_topic == "Sexual Abstinence" &
                                                            post_tone == "Enact fear"), 20))

  	

## Cross Tabulation for Style
#### See::https://jrnold.github.io/ggthemes/reference/tableau_color_pal.html:: for palette




ggplot(style_data, aes(fill = factor(post_style,
                                     labels = c("Counsel",
                                                "Experience sharing",
                                                "Request for Opinion",
                                                "Storytelling")),
                       x = perc_style,
                       y = factor(post_topic,
                                  labels = c("Abuse",
                                             "Family\nPlanning",
                                             "Intimate\nRelations",
                                             "Sexual\nAbstinence",
                                             "Sexual\nPurity")))) +
        geom_bar(position="fill", stat="identity") +
        scale_fill_manual(values = c("#A8A8A8", "#ffb612", "#000000", "#0B77A1")) + 
        scale_x_continuous(labels = function(x) x * 100,
                           expand = c(0,0)) +
        labs(
          # title = "Percentage Distribution of Topics by Style and Tone",
          subtitle = "(a) Topic Classifications by Purpose of Communication",
          y = "",
          x = "Percentage Distribution (%)",
          fill = "Post Tone"
        ) + 
        theme_set -> style





ggplot(tone_data, aes(fill = factor(post_tone,
                                    labels = c("Enact\nFear",
                                               "Guilt",
                                               "Neutral",
                                               "Stigma")),
                      x = perc_tone,
                      y = factor(post_topic,
                                 labels = c("Abuse",
                                            "Family\nPlanning",
                                            "Intimate\nRelations",
                                            "Sexual\nAbstinence",
                                            "Sexual\nPurity")))) +
          geom_bar(position="fill", stat="identity") +
          scale_fill_manual(values = c("#A8A8A8", "#ffb612", "#000000", "#0B77A1")) + 
          scale_x_continuous(labels = function(x) x * 100,
                             expand = c(0,0)) +
          labs(
            # title = "Percentage Distribution of Topics by Style and Tone",
            subtitle = "(b) Topic Classifications by Tone of Communication",
            y = "",
            x = "Percentage Distribution (%)",
            fill = "Post Tone") +
          theme_set -> tone




ggarrange(style, tone, ncol = 1, nrow = 2, align = "v", legend = "right")

ggsave("./output/Topics_by_Tone-Style.png", dpi = 250, height = 9, width = 15)



