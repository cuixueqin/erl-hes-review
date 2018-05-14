library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
theme_set(theme_bw())

DATA <- read_csv("./data_modelcomplexity.csv")

feedbacks.color <- c( "None" = "#8da0cb", "One-Way" = "#fc8d62", "Two-Way" = "#66c2a5")
FillScale <- scale_fill_manual(name = "Feedbacks", values = feedbacks.color)  

included.color <- c( "YES" = "black", "NO" = "gray")
ColScale <- scale_color_manual(name = "Included", values = included.color)  

xArrowLab <- data.frame(lab = c("Simple","Complex"),
                       x = c(0, 12), 
                       y = c(-1, -1))
yArrowLab <- data.frame(lab = c("Simple","Complex"),
                        x = c(-1, -1), 
                        y = c(0, 12))

p <- ggplot() + geom_point(data=DATA, aes(Human.Complexity, Earth.Complexity, fill=Feedbacks, color=Included), shape=21, size = 5, stroke = 1.5, stat="identity")
p <- p + geom_text(data=DATA, aes(Human.Complexity, Earth.Complexity, label=Model), hjust=-0.25, vjust=0, angle=45)
p <- p + geom_segment(aes(x=0, xend = 12, y=-1, yend=-1), size=1.5, arrow = arrow(length = unit(0.4,"cm"), ends="both")) 
p <- p + geom_text(data=xArrowLab, aes(x, y, label=lab), hjust=0.5, vjust=2)
p <- p + geom_segment(aes(x=-1, xend = -1, y=0, yend=12), size=1.5, arrow = arrow(length = unit(0.4,"cm"), ends="both")) 
p <- p + geom_text(data=yArrowLab, aes(x, y, label=lab), hjust=0.5, vjust=-2, angle=90)
p <- p + ylab("Earth System Complexity") + xlab("Human System Complexity")
p <- p + FillScale + ColScale
p <- p + xlim(-2, 12.5) + ylim(-1, 12.5)
p <- p + theme(axis.text=element_blank(),
               axis.ticks=element_blank(),
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank())
print(p)
ggsave("Figure4.png", width=8, height=6)

