############################
#### EXAMPLES ICE PLANE ####
############################
rm(list = ls())
options(scipen = 999)

library(tidyverse)
library(scales)
library(rstudioapi)
setwd(dirname(getActiveDocumentContext()$path)) 

rects <- data.frame(xstart = c(0, 0, 0, 0), xend = c(Inf, Inf, -Inf, -Inf),
                    ystart = c(0, 0, 0, 0), yend = c(Inf, -Inf, -Inf, Inf), 
                    col = c("NE", "SE", "SW", "NW")
                    )

df_dots <- data.frame(cbind(
  x = c(4, 1),
  y = c(15000, 30000),
  labs = c(1, 2)
))

shift_axis <- function(p, y=0){
  g <- ggplotGrob(p)
  dummy <- data.frame(y=y)
  ax <- g[["grobs"]][g$layout$name == "axis-b"][[1]]
  p + annotation_custom(grid::grobTree(ax, vp = grid::viewport(y=1, height=sum(ax$height))), 
                        ymax=y, ymin=y) +
    geom_hline(aes(yintercept=y), data = dummy) +
    theme(axis.text.x = element_blank(), 
          axis.ticks.x=element_blank())
  
}

shift_axis_x <- function(p, x=0){
  g <- ggplotGrob(p)
  dummy <- data.frame(x=x)
  ax <- g[["grobs"]][g$layout$name == "axis-l"][[1]]
  p + annotation_custom(grid::grobTree(ax, vp = grid::viewport(x=1, width = sum(ax$height))), 
                        xmax=x, xmin=x) +
    geom_vline(aes(xintercept=x), data = dummy) +
    theme(axis.text.y = element_blank(), 
          axis.ticks.y=element_blank())
}

p <- ggplot() +
  xlim(-5, 5) + 
  #ylim() +
  xlab ("Incremental effects (e.g. QALYs)") + 
  ylab("Incremental costs") +
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0) +
  geom_abline(intercept = 0, 
              slope = 20000, 
              linetype= "dashed",
              color = "black") + # 20,000 per QALY threshold line
  scale_y_continuous(limits = c(-40000, 40000),
                      labels = dollar_format(prefix = "\u20ac ", suffix = "")) +
  theme_bw()
p <- p + geom_rect(data = rects, aes(xmin = xstart, xmax = xend, ymin = ystart, ymax = yend, fill = col),
                   alpha = 0.5) +
  scale_fill_manual(name = "",
                    values = c(NE = "lightblue", 
                               SE = "darkgreen", 
                               SW = "lightgrey", 
                               NW = "darkred")) +
  annotate("text", 
  x = -2.5,  
  y = 20000,   
  label = "Intervention
          dominated (reject)",
  size = 6) +
  annotate("text", 
           x = 2.5,  
           y = -20000,   
           label = 
           "Intervention
            dominates (accept)",
           size = 6) +
  annotate("text", 
           x = 2.5,  
           y = 20000,   
           label = "Trade-off",
           size = 6) +
  annotate("text", 
           x = -2.5,  
           y = -20000,   
           label = "Trade-off",
           size = 6)

p <- shift_axis(p)
p <- shift_axis_x(p)
#p

p_dots <- p +
  geom_point(data = df_dots, aes(x = x, y = y), size = 3) +
  geom_label(data = df_dots,
    aes(x = x, y = y, label = labs),
    nudge_x = 0.3,
    nudge_y = 2000)
#p_dots

# Export
tiff("iCEplane.tif", width = 20, height = 20*9/16, units = "cm", res = 600)
p
dev.off()

tiff("iCEplane_interventions.tif", width = 20, height = 20*9/16, units = "cm", res = 600)
p_dots
dev.off()

