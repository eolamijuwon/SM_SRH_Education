################################################################################
#
## Advancing sexual health education for young African adults in the digital age: 
## Uncovering strategies for successful organic engagement
#
# Descriptive Characteristics and Regression Models
#
################################################################################




## Read Datasets from GitHub              
FB_SRH <- read.csv("./Data/FB_SRH.csv")
codebook <- read.csv("./Data/codebook.csv")



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
  save_kable(file = "./output/Descriptive.html",
             self_contained = T, bs_theme = "simplex")







interactions <- data.frame(rbind(cbind(FB_SRH$post_reactions, "Post Reactions"),
                                 cbind(FB_SRH$post_shares, "Post Shares"),
                                 cbind(FB_SRH$post_comments, "Post Comments"))
                           ) %>% 
                rename(count = X1, category = X2) %>% 
                mutate(count = as.numeric(as.character(count)))




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
          out = "./output/Poisson_Models.html")



