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
library(viridisLite)



theme_set(theme_classic())
output_dir <- "../output/"
data_dir <- "../data/"


#               mentor, coach, instr, lead, mentee
role_colors = c("red", "green", "purple", "yellow", "blue")
#roles = c("mentor", "mentee", "instr", "lead", "coach")
gender2_colors = c("grey", "pink", "cyan")
gender3_colors = c("pink", "cyan")
dyad_colors = c("black", "red")


#role_pallete <- brewer.pal(5,"Set1")
role_pallete <- role_colors
#names(role_pallete) <- levels(participants$role)
role_fill <- scale_fill_manual(name = "role", values = role_pallete)
role_color <- scale_color_manual(name = "role", values = role_pallete)
sender_role_fill <- scale_fill_manual(name = "sender_role", values = role_pallete)
sender_role_color <- scale_color_manual(name = "sender_role", values = role_pallete)
receiver_role_fill <- scale_fill_manual(name = "receiver_role", values = role_pallete)
receiver_role_color <- scale_color_manual(name = "receiver_role", values = role_pallete)

dyad_pallete <- dyad_colors
names(dyad_pallete) <- c(FALSE, TRUE)
dyad_fill <- scale_fill_manual(name = "dyad", values = dyad_pallete)
dyad_color <- scale_color_manual(name = "dyad", values = dyad_pallete)