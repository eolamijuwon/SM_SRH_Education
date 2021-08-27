################################################################################
#
#' Understanding how young African adults interact with 
#' peer-generated sexual health information on Facebook and 
#' uncovering strategies for successful organic engagement
#' 
#  ########################################################
#' 
#  Message Features and Regression Models
#
################################################################################




## Read Datasets from GitHub              
FB_SRH <- read.csv("Data/FB_SRH.csv") %>% 
          mutate(post_style = as.factor(post_style),
                 post_topic = as.factor(post_topic),
                 post_tone = as.factor(post_tone),
                 post_type = as.factor(post_type)) %>% 
          dplyr::select(-c("X"))

codebook <- read.csv("Data/codebook.csv")



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
                 mutate(Percentage = round(((Freq/sum(Freq))*100), digits = 1))),
              
              (table(FB_SRH$post_type) %>% 
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
              kableExtra::group_rows("Strategy", 6, 9) %>% 
              kableExtra::group_rows("Tone", 10, 13) %>% 
              kableExtra::group_rows("Type", 14, 16) 


Descriptive %>% 
  save_kable(file = "Output/Descriptive.html",
             self_contained = T, bs_theme = "simplex")







interactions <- data.frame(rbind(cbind(FB_SRH$post_reactions, "Post Reactions"),
                                 cbind(FB_SRH$post_shares, "Post Shares"),
                                 cbind(FB_SRH$post_comments, "Post Comments"))
                           ) %>% 
                rename(count = X1, category = X2) %>% 
                mutate(count = as.numeric(as.character(count)))




# summary(model_shares)



#####


signif.num <- function(x) {
  symnum(x, corr = FALSE, na = FALSE, legend = FALSE,
         cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
         symbols = c("***", "**", "*", " "))
}



#######

model_total <- glm.nb(total_inter ~ 
                     relevel(post_topic, ref = "Intimate Relations") +
                     relevel(post_tone, ref = "Neutral") +
                     relevel(post_style, ref = "Status update") +
                     relevel(post_type, ref = "Text only"),
                   
                   data = FB_SRH)



mod_total <- exp(cbind(IRR = coef(model_total), confint(model_total))) %>% 
              as.data.frame()
              pval <- (summary(model_total)$coefficients[,4])
              total_pv <- as.data.frame(signif.num(pval))
              rownames (total_pv) <- NULL
              
            mod_total <-  mod_total %>%
              add_rownames("Characteristics") %>%
              mutate ("IRR" = round(IRR, digits = 2)) %>% 
              mutate ("LCI" = round(`2.5 %`, digits = 2)) %>% 
              mutate ("UCI" = round(`97.5 %`, digits = 2)) %>% 
              mutate ("Characteristics" = c("Intercept", "Abuse",
                                            "Birth Control and Abortion",
                                            "Sexual Abstinence", "Sexual Purity",
                                            "Enact fear", "Guilt", "Stigma", 
                                            "Experience sharing",
                                            "Request for opinion", "Storytelling",
                                            "Link", "Photo"))
                          mod_total <- cbind(mod_total, total_pv) %>% 
                                        rename(pval = `signif.num(pval)`) %>% 
                                        mutate(IRR = paste(IRR, pval, sep = ""),
                                               CI = paste(LCI, " - ", UCI, sep = "")) %>% 
                                        dplyr::select("Characteristics", "IRR", "CI")

## Number of Shares ##
                          
model_shares <- glm.nb(post_shares ~ relevel(post_topic, ref = "Intimate Relations") +
                         relevel(post_tone, ref = "Neutral") +
                         relevel(post_style, ref = "Status update") +
                         relevel(post_type, ref = "Text only"),
                       data = FB_SRH)

###########                          
mod_share <- exp(cbind(IRR = coef(model_shares), confint(model_shares))) %>% 
              as.data.frame()
              pval <- (summary(model_shares)$coefficients[,4])
              share_pv <- as.data.frame(signif.num(pval))
              rownames (share_pv) <- NULL
            
            mod_share <-  mod_share %>%
              add_rownames("Characteristics") %>%
              mutate ("IRR" = round(IRR, digits = 2)) %>% 
              mutate ("LCI" = round(`2.5 %`, digits = 2)) %>% 
              mutate ("UCI" = round(`97.5 %`, digits = 2)) %>% 
              mutate ("Characteristics" = c("Intercept", "Abuse",
                                "Birth Control and Abortion",
                                "Sexual Abstinence", "Sexual Purity",
                                "Enact fear", "Guilt", "Stigma", 
                                "Experience sharing",
                                "Request for opinion", "Storytelling",
                                "Link", "Photo"))
                          mod_share <- cbind(mod_share, share_pv) %>% 
                            rename(pval = `signif.num(pval)`) %>% 
                            mutate(IRR = paste(IRR, pval, sep = ""),
                                   CI = paste(LCI, " - ", UCI, sep = "")) %>% 
                            dplyr::select("Characteristics", "IRR", "CI")
                          
## Number of Comments ####      
                          
model_comments <- glm.nb(post_comments ~ relevel(post_topic, ref = "Intimate Relations") +
                           relevel(post_tone, ref = "Neutral") +
                           relevel(post_style, ref = "Status update") +
                           relevel(post_type, ref = "Text only"),
                         
                      data = FB_SRH)

#### 
mod_comment <- exp(cbind(IRR = coef(model_comments), confint(model_comments))) %>% 
              as.data.frame()
              pval <- (summary(model_comments)$coefficients[,4])
              comment_pv <- as.data.frame(signif.num(pval))
              rownames (comment_pv) <- NULL
            
            mod_comment <-  mod_comment %>%
              add_rownames("Characteristics") %>%
              mutate ("IRR" = round(IRR, digits = 2)) %>% 
              mutate ("LCI" = round(`2.5 %`, digits = 2)) %>% 
              mutate ("UCI" = round(`97.5 %`, digits = 2)) %>% 
              mutate ("Characteristics" = c("Intercept", "Abuse",
                                            "Birth Control and Abortion",
                                            "Sexual Abstinence", "Sexual Purity",
                                            "Enact fear", "Guilt", "Stigma", 
                                            "Experience sharing",
                                            "Request for opinion", "Storytelling",
                                            "Link", "Photo"))
          mod_comment <- cbind(mod_comment, comment_pv) %>% 
                            rename(pval = `signif.num(pval)`) %>% 
                            mutate(IRR = paste(IRR, pval, sep = ""),
                                   CI = paste(LCI, " - ", UCI, sep = "")) %>% 
                            dplyr::select("Characteristics", "IRR", "CI")
                    
     
          
####          
model_reactions <- glm.nb(post_reactions ~ relevel(post_topic, ref = "Intimate Relations") +
                            relevel(post_tone, ref = "Neutral") +
                            relevel(post_style, ref = "Status update") +
                            relevel(post_type, ref = "Text only"),
                          data = FB_SRH)
          
          
#### 
mod_reaction <- exp(cbind(IRR = coef(model_reactions), confint(model_reactions))) %>% 
                as.data.frame()
                pval <- (summary(model_reactions)$coefficients[,4])
                reaction_pv <- as.data.frame(signif.num(pval))
                rownames (total_pv) <- NULL
          
          mod_reaction <-  mod_reaction %>%
                add_rownames("Characteristics") %>%
                mutate ("IRR" = round(IRR, digits = 2)) %>% 
                mutate ("LCI" = round(`2.5 %`, digits = 2)) %>% 
                mutate ("UCI" = round(`97.5 %`, digits = 2)) %>% 
                mutate ("Characteristics" = c("Intercept", "Abuse",
                                              "Birth Control and Abortion",
                                              "Sexual Abstinence", "Sexual Purity",
                                              "Enact fear", "Guilt", "Stigma", 
                                              "Experience sharing",
                                              "Request for opinion", "Storytelling",
                                              "Link", "Photo"))
              mod_reaction <- cbind(mod_reaction, reaction_pv) %>% 
                rename(pval = `signif.num(pval)`) %>% 
                mutate(IRR = paste(IRR, pval, sep = ""),
                       CI = paste(LCI, " - ", UCI, sep = "")) %>% 
                dplyr::select("Characteristics", "IRR", "CI")
          
mod_total <- mod_total %>% 
             left_join(mod_reaction, by = "Characteristics") %>% 
             left_join(mod_comment, by = "Characteristics") %>% 
             left_join(mod_share, by = "Characteristics") %>% 
             filter(Characteristics != "Intercept")

names (mod_total) <- sub("\\..*", "", names(mod_total))
names (mod_total) <- sub("CI", "95% CI", names(mod_total))

          mod_total %>% 
          kable(caption = " ", align = "l",
                booktabs = T, position = "centre") %>%
          kable_styling(full_width = F,  row_label_position="l") %>%
          add_header_above(c("", "Total Interactions" = 2,
                             "Number of Reactions" = 2,
                             "Number of Comments" = 2, 
                             "Number of Shares" = 2 )) %>% 
  save_kable(file = "Output/regression_model.html",
             self_contained = T, bs_theme = "simplex")

  
          
          
stargazer(model_total, model_reactions, 
          model_comments, model_shares, 
          type = "text", digits = 2, 
          single.row = TRUE, ci = TRUE, 
          p.auto=F, align = TRUE,
          apply.coef = exp, 
          star.cutoffs = c(0.05, 0.01, 0.001),
          covariate.labels=c("Abuse",
                             "Birth Control and Abortion",
                             "Sexual Abstinence", "Sexual Purity",
                             "Enact fear", "Guilt", "Stigma", 
                             "Experience sharing",
                             "Request for opinion", "Storytelling",
                             "Link", "Photo"),
          out = "Output/Poisson_Models.html")



