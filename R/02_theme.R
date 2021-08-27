################################################################################
#
## Advancing sexual health education for young African adults in the digital age: 
## Uncovering strategies for successful organic engagement
#
# Set Theme and Import Fonts
#
################################################################################


# NOTE! you might still need to istall the font manually on your OS
## You can download the font family free from 1001 Fonts
## https://www.1001fonts.com/roboto-condensed-font.html

showtext_auto()

if ("Montserrat-Regular.ttf" %in% list.files("C:\\Windows\\Fonts")) {
  
font_add("Montserrat", "Montserrat-Regular.ttf")
Font <- "Montserrat"

} else {
  
  font_add("ARIALN", "ARIALN.ttf")
  Font <- "ARIALN"
  
}


########
theme_set <- 
  
      theme_bw() + 
      
      theme(
        
        # Legend Settings
        
        legend.title = element_text(size = 32, face = "bold", 
                                    family = Font,
                                    lineheight = unit(0.1, "pt")),
        legend.text = element_text(size = 30, 
                                   family = Font,
                                   lineheight = unit(0.4, "pt")),
        # legend.key = element_rect(size = 1, "pt"),
        legend.background = element_blank(),
        legend.justification = "left",
        legend.box.margin = margin(0, 0, 0, 0),
        # legend.position = "bottom",
        # legend.direction = "horizontal",
        # legend.box = "vertical",
        
        # Backgrounds
        # strip.background = element_rect(fill = "black"),
        # strip.text = element_text(size = 35, face = "bold", 
        #                           family = Font, colour  = "white"),
        plot.background = element_blank(),
        plot.margin = margin(0.5,0.5,1.0,1.0, unit = "cm"),
        plot.title = element_text(size = 45, face = "bold", 
                                  family = Font, 
                                  lineheight = unit(0.4, "pt")),
        plot.subtitle = element_text(size = 37, face = "bold", 
                                     family = Font, 
                                     lineheight = unit(0.4, "pt")),
        
        # Axis & Titles
        axis.line = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x=element_text(size = 32, face = "bold",
                                 family = Font),
        
        axis.title.y = element_text(size = 28, face = "bold",
                                    family = Font),
        axis.text.y = element_text(size = 32, face = "bold",
                                   family = Font,
                                   lineheight = unit(0.4, "pt")),
        
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
    
