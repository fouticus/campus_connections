library(ggplot2)
library(RColorBrewer)

library(igraph)

library(gmodels)
library(gtools)
library(stringr)
library(plyr)
library(reshape2)
library(grid)
library(gridExtra)



theme_set(theme_classic())
output_dir <- "../output/"
data_dir <- "../data/"


#               mentor, coach, instr, lead, mentee
role_colors = c("red", "green", "purple", "yellow", "blue")
#roles = c("mentor", "mentee", "instr", "lead", "coach")
gender2_colors = c("grey", "pink", "cyan")
gender3_colors = c("pink", "cyan")


#role_pallete <- brewer.pal(5,"Set1")
role_pallete <- role_colors
names(role_pallete) <- levels(participants$role)
role_fill <- scale_fill_manual(name = "role", values = role_pallete)
