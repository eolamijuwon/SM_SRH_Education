
### Title: Advancing SexEd for Young African Adults

### Author 1: *************
### Author 2: *************
### Author 3: *************




# Loading/installing required packages

pkgs <- c("tidyverse",
          "waffle",
          "lubridate",
          "tidytext",
          "extrafont",
          "showtext",
          "kableExtra",
          "stargazer",
          "scales",      # for automatically determining breaks and labels for axes and legends.
          "ggpubr",      # Publication Ready Plots
          "ggpol",       # Data Visualization - geom_boxjitter
          "stringr",
          "htmlwidgets",
          "tweenr",
          "webshot",
          "readxl",
          "MASS"
          )

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


          signif.num <- function(x) {
            symnum(x, corr = FALSE, na = FALSE, legend = FALSE,
                   cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                   symbols = c("***", "**", "*", " "))
          }
              
              
## Read Datasets from GitHub              
FB_SRH <- read.csv(text=getURL("https://raw.githubusercontent.com/eolamijuwon/advancing-SexEd/master/FB-Advancing%20SRH.csv"), header=T)
codebook <- read.csv(text=getURL("https://raw.githubusercontent.com/eolamijuwon/advancing-SexEd/master/SRH%20-%20codebook.csv"), header=T)



## Coding scheme and frequencies of topics raised in the posts, style and tone of posts

Descriptive <- rbind(
              (table(FB_SRH$post_topic) %>% 
                 data.frame() %>% 
                 mutate(Percentage = round(((Freq/sum(Freq))*100), digits = 1))),
              
              (table(FB_SRH$post_style) %>% 
                 data.frame() %>% 
                 mutate(Percentage = round(((Freq/sum(Freq))*100), digits = 1))),
              
              (table(FB_SRH$post_tone) %>% 
                 data.frame() %>% 
                 mutate(Percentage = round(((Freq/sum(Freq))*100), digits = 1)))
              ) %>% 
              rename(Classification = Var1) %>% 
              inner_join(codebook,
                         by = c("Classification")) %>% 
              dplyr::select(c("Classification", "Explanation", "Example",
                       "Freq", "Percentage")) %>% 
              kable(align = "l", booktabs = T, position = "left") %>%
              kable_styling("striped", full_width = F) %>%
              kableExtra::group_rows("Topics", 1, 5) %>% 
              kableExtra::group_rows("Style", 6, 9) %>% 
              kableExtra::group_rows("Tone", 10, 13)


Descriptive %>% 
  save_kable(file = "../figures/Descriptive.html",
             self_contained = T, bs_theme = "simplex")




## Theme Modifications

showtext_auto()
font_add("RobotoCondensed", "RobotoCondensed-Light.ttf")
RobotoCondensed <- "RobotoCondensed"

theme_JAH <- 
              theme_bw() + 
  
              theme(
                
                # Legend Settings
                
                legend.title = element_text(size = 23),
                legend.text = element_text(size = 30, face = "bold", 
                                           family = RobotoCondensed,
                                           lineheight = unit(0.4, "pt")),
                legend.key = element_blank(),
                legend.background = element_blank(),
                legend.position = "bottom",
                # legend.direction = "horizontal",
                # legend.box = "vertical",
                
                # Backgrounds
                strip.background = element_rect(fill = "black"),
                strip.text = element_text(size = 35, face = "bold", 
                                          family = RobotoCondensed, colour  = "white"),
                plot.background = element_blank(),
                plot.margin = margin(0.5,0.5,1.0,1.0, unit = "cm"),
                plot.title = element_text(size = 45, face = "bold", 
                                          family = RobotoCondensed, 
                                          lineheight = unit(0.4, "pt")),
                plot.subtitle = element_text(size = 37, face = "bold", 
                                             family = RobotoCondensed, 
                                             lineheight = unit(0.4, "pt")),
                
                # Axis & Titles
                axis.line = element_blank(),
                axis.title.y = element_text(size = 28, face = "bold",
                                            family = RobotoCondensed, 
                                            angle=90, vjust=2.5),
                axis.text.y=element_text(size = 28, face = "bold",
                                         family = RobotoCondensed),
                
                axis.title.x = element_text(size = 28, face = "bold",
                                            family = RobotoCondensed,
                                            vjust = -2.5),
                axis.text.x=element_text(size = 28, face = "bold",
                                         family = RobotoCondensed),
                
                # Panel
                panel.grid = element_line(colour = NULL),
                panel.grid.major.y = element_line(colour = "#D2D2D2",
                                                  linetype = "dashed",
                                                  size = 0.3),
                panel.grid.major.x = element_line(colour = "#D2D2D2",
                                                  linetype = "dashed",
                                                  size = 0.3),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                panel.border = element_blank())




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
                theme_JAH +
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
                                                  family = RobotoCondensed,
                                                  lineheight = unit(0.25, "pt"),
                                                  face = "italic"))
              
ggsave("../figures/Social Interactions.png", dpi = 300, height = 6.5, width = 4)



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
                         values = perc_style)) +
              geom_waffle(color = "white", size = .45, n_rows = 10, flip = TRUE) +
              facet_wrap(~factor(post_topic,
                                 labels = c("Abuse",
                                            "Family\nPlanning",
                                            "Intimate\nRelations",
                                            "Sexual\nAbstinence",
                                            "Sexual\nPurity")),
                         nrow = 1, strip.position = "bottom") +
              scale_x_discrete() +
              scale_y_continuous(labels = function(x) x * 10,
                                 expand = c(0,0)) +
              ggthemes::scale_fill_tableau("Color Blind", name=NULL) +
              labs(
                title = "Percentage Distribution of Topics by Style and Tone",
                subtitle = "(a) Topic Classifications by Style of Communication",
                x = "",
                y = "Percentage Distribution (%)"
              ) +
              theme_JAH +
              theme(panel.grid = element_blank(), 
                    axis.ticks.y = element_line(),
                    strip.text.x = element_blank(),
                    axis.title.y = element_text(margin = unit(c(0, -0.5, 0, 0), "cm")),
                    # plot.title = element_text(size = 45, 
                    #                           lineheight = unit(0.4, "pt")),
                    plot.title = element_blank(),
                    plot.subtitle = element_text(size = 35,
                                                 margin = unit(c(0, 0, 0.5, 0), "cm")),
                    legend.text = element_text(size = 23,
                                               lineheight = unit(0.4, "pt")),
                    legend.title = element_text(size = 23),
                    legend.justification = "left",
                    legend.box.margin = margin(0, 0, 0, 0)
              ) + guides(fill = guide_legend(reverse = TRUE)) -> style


## Cross Tabulation for Tone
ggplot(tone_data, aes(fill = factor(post_tone,
                                    labels = c("Enact\nFear",
                                               "Guilt",
                                               "Neutral",
                                               "Stigma")),
                      values = perc_tone)) +
              geom_waffle(color = "white", size = .45, n_rows = 10, flip = TRUE) +
              facet_wrap(~factor(post_topic,
                                 labels = c("Abuse",
                                            "Family\nPlanning",
                                            "Intimate\nRelations",
                                            "Sexual\nAbstinence",
                                            "Sexual\nPurity")),
                         nrow = 1, strip.position = "bottom") +
              scale_x_discrete() +
              scale_y_continuous(labels = function(x) x * 10,
                                 expand = c(0,0)) +
              
              ggthemes::scale_fill_tableau("Color Blind", name=NULL) +
              
              labs(
                title = "",
                subtitle = "(b) Topic Classifications by Tone of Communication",
                x = "Topics",
                y = "Percentage Distribution (%)"
              ) +
              theme_JAH +
              theme(panel.grid = element_blank(), 
                    axis.ticks.y = element_line(),
                    # axis.text.y = element_text(size = 23),
                    # axis.title.y = element_text(size = 23,
                    #                             margin = unit(c(0, -0.5, 0, 0),
                    #                                           "cm")),
                    # axis.text.x = element_text(size = 23),
                    axis.title.y = element_text(margin = unit(c(0, -0.5, 0, 0), "cm")),
                    strip.text.x = element_text(size = 23, 
                                                colour  = "black",
                                                lineheight = unit(0.4, "pt")),
                    
                    strip.background = element_blank(),
                    
                    plot.title = element_blank(),
                    plot.subtitle = element_text(size = 35,
                                                 margin = unit(c(0, 0, 0.5, 0), "cm")),
                    legend.text = element_text(size = 23, 
                                               lineheight = unit(0.4, "pt")),
                    legend.title = element_text(size = 23),
                    legend.justification = "left",
                    legend.box.margin = margin(0, 0, 0, 0)
              ) + guides(fill = guide_legend(reverse = TRUE)) -> tone

ggarrange(style, tone, ncol = 1, nrow = 2, align = "v", legend = "right")

ggsave("../figures/Topics_by_Tone-Style.png", dpi = 250, height = 9, width = 10)






# summary(model_shares)

model_total <- glm.nb(total_inter ~ 
                     relevel(post_topic, ref = "Intimate Relations") +
                     relevel(post_tone, ref = "Neutral") +
                     relevel(post_style, ref = "Counsel") +
                     relevel(post_type, ref = "status"),
                   
                   data = FB_SRH)



mod_total <- exp(cbind(OR = coef(model_total), confint(model_total))) %>% 
              as.data.frame()
              pval <- (summary(model_total)$coefficients[,4])
              total_pv <- as.data.frame(signif.num(pval))
              rownames (total_pv) <- NULL
              
            mod_total <-  mod_total %>%
              add_rownames("Characteristics") %>%
              mutate ("OR" = round(OR, digits = 2)) %>% 
              mutate ("LCI" = round(`2.5 %`, digits = 2)) %>% 
              mutate ("UCI" = round(`97.5 %`, digits = 2)) %>% 
              mutate ("Characteristics" = c("Intercept", "Abuse",
                                            "Family Planning",
                                            "Sexual Abstinence", "Sexual Purity",
                                            "Enact fear", "Guilt", "Stigma", 
                                            "Experience sharing",
                                            "Request for opinion", "Storytelling",
                                            "Link", "Photo"))
                          mod_total <- cbind(mod_total, total_pv) %>% 
                                        rename(pval = `signif.num(pval)`) %>% 
                                        mutate(OR = paste(OR, pval, sep = ""),
                                               CI = paste(LCI, " - ", UCI, sep = "")) %>% 
                                        dplyr::select("Characteristics", "OR", "CI")

## Number of Shares ##
                          
model_shares <- glm.nb(post_shares ~ relevel(post_topic, ref = "Intimate Relations") +
                         relevel(post_tone, ref = "Neutral") +
                         relevel(post_style, ref = "Counsel") +
                         relevel(post_type, ref = "status"),
                       data = FB_SRH)

###########                          
mod_share <- exp(cbind(OR = coef(model_shares), confint(model_shares))) %>% 
              as.data.frame()
              pval <- (summary(model_shares)$coefficients[,4])
              share_pv <- as.data.frame(signif.num(pval))
              rownames (share_pv) <- NULL
            
            mod_share <-  mod_share %>%
              add_rownames("Characteristics") %>%
              mutate ("OR" = round(OR, digits = 2)) %>% 
              mutate ("LCI" = round(`2.5 %`, digits = 2)) %>% 
              mutate ("UCI" = round(`97.5 %`, digits = 2)) %>% 
              mutate ("Characteristics" = c("Intercept", "Abuse",
                                "Family Planning",
                                "Sexual Abstinence", "Sexual Purity",
                                "Enact fear", "Guilt", "Stigma", 
                                "Experience sharing",
                                "Request for opinion", "Storytelling",
                                "Link", "Photo"))
                          mod_share <- cbind(mod_share, share_pv) %>% 
                            rename(pval = `signif.num(pval)`) %>% 
                            mutate(OR = paste(OR, pval, sep = ""),
                                   CI = paste(LCI, " - ", UCI, sep = "")) %>% 
                            dplyr::select("Characteristics", "OR", "CI")
                          
## Number of Comments ####      
                          
model_comments <- glm.nb(post_comments ~ relevel(post_topic, ref = "Intimate Relations") +
                           relevel(post_tone, ref = "Neutral") +
                           relevel(post_style, ref = "Counsel") +
                           relevel(post_type, ref = "status"),
                         
                      data = FB_SRH)

#### 
mod_comment <- exp(cbind(OR = coef(model_comments), confint(model_comments))) %>% 
              as.data.frame()
              pval <- (summary(model_comments)$coefficients[,4])
              comment_pv <- as.data.frame(signif.num(pval))
              rownames (comment_pv) <- NULL
            
            mod_comment <-  mod_comment %>%
              add_rownames("Characteristics") %>%
              mutate ("OR" = round(OR, digits = 2)) %>% 
              mutate ("LCI" = round(`2.5 %`, digits = 2)) %>% 
              mutate ("UCI" = round(`97.5 %`, digits = 2)) %>% 
              mutate ("Characteristics" = c("Intercept", "Abuse",
                                            "Family Planning",
                                            "Sexual Abstinence", "Sexual Purity",
                                            "Enact fear", "Guilt", "Stigma", 
                                            "Experience sharing",
                                            "Request for opinion", "Storytelling",
                                            "Link", "Photo"))
          mod_comment <- cbind(mod_comment, comment_pv) %>% 
                            rename(pval = `signif.num(pval)`) %>% 
                            mutate(OR = paste(OR, pval, sep = ""),
                                   CI = paste(LCI, " - ", UCI, sep = "")) %>% 
                            dplyr::select("Characteristics", "OR", "CI")
                    
     
          
####          
model_reactions <- glm.nb(post_reactions ~ relevel(post_topic, ref = "Intimate Relations") +
                            relevel(post_tone, ref = "Neutral") +
                            relevel(post_style, ref = "Counsel") +
                            relevel(post_type, ref = "status"),
                          data = FB_SRH)
          
          
#### 
mod_reaction <- exp(cbind(OR = coef(model_reactions), confint(model_reactions))) %>% 
                as.data.frame()
                pval <- (summary(model_reactions)$coefficients[,4])
                reaction_pv <- as.data.frame(signif.num(pval))
                rownames (total_pv) <- NULL
          
          mod_reaction <-  mod_reaction %>%
                add_rownames("Characteristics") %>%
                mutate ("OR" = round(OR, digits = 2)) %>% 
                mutate ("LCI" = round(`2.5 %`, digits = 2)) %>% 
                mutate ("UCI" = round(`97.5 %`, digits = 2)) %>% 
                mutate ("Characteristics" = c("Intercept", "Abuse",
                                              "Family Planning",
                                              "Sexual Abstinence", "Sexual Purity",
                                              "Enact fear", "Guilt", "Stigma", 
                                              "Experience sharing",
                                              "Request for opinion", "Storytelling",
                                              "Link", "Photo"))
              mod_reaction <- cbind(mod_reaction, reaction_pv) %>% 
                rename(pval = `signif.num(pval)`) %>% 
                mutate(OR = paste(OR, pval, sep = ""),
                       CI = paste(LCI, " - ", UCI, sep = "")) %>% 
                dplyr::select("Characteristics", "OR", "CI")
          
mod_total %>% 
          left_join(mod_reaction, by = "Characteristics") %>% 
          left_join(mod_comment, by = "Characteristics") %>% 
          left_join(mod_share, by = "Characteristics") %>% 
          filter(Characteristics != "Intercept") %>% 
          rename(`95% CI` = `CI.x`,
                 `95% CI` = `CI.y`,
                 `95% CI` = `CI.x.x`,
                 `95% CI` = `CI.y.y`,
                 OR = `OR.x`,
                 OR = `OR.y`,
                 OR = `OR.x.x`,
                 OR = `OR.y.y`) %>% 
          kable(caption = " ", align = "l",
                booktabs = T, position = "centre") %>%
          kable_styling(full_width = F,  row_label_position="l") %>%
          add_header_above(c("", "Total Interactions" = 2,
                             "Number of Reactions" = 2,
                             "Number of Comments" = 2, 
                             "Number of Shares" = 2 )) %>% 
  save_kable(file = "../figures/regression_model.html",
             self_contained = T, bs_theme = "simplex")

  
          
          
stargazer(model_total, model_reactions, 
          model_comments, model_shares, 
          type = "text", digits = 2, 
          single.row = TRUE, ci = FALSE, 
          #ci.level = 0.95, 
          # ci.separator = " - ",
          covariate.labels=c("Abuse",
                             "Family Planning",
                             "Sexual Abstinence", "Sexual Purity",
                             "Enact fear", "Guilt", "Stigma", 
                             "Experience sharing",
                             "Request for opinion", "Storytelling",
                             "Link", "Photo"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          keep.stat = c("n","ll"),
          out = "../figures/Poisson_Models.html")



