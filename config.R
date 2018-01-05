########################################################################################
## config.R
## Loads all necessary R packages, specifies directories, and some plot settings
########################################################################################

# R Packages
library(R6)
library(rstackdeque)
library(ggplot2)
library(RColorBrewer)
library(igraph)
library(gmodels)
library(gtools)
library(stringr)
library(plyr)
library(dplyr)
library(reshape2)
library(grid)
library(gridExtra)
library(viridisLite)
library(openxlsx)
library(hashmap)
library(parallel)
library(abind)

### Directories
output_dir <- "../output/"
data_dir <- "../data/"


### Which semesters and nights we are looking at right now.
semesters <- c("Fa15", "Sp16", "Fa16", "Sp17")
nights <- c("Mon", "Tue", "Wed", "Thu")


### Plot settings
theme_set(theme_classic())
role_labels = c("mentor", "mentee", "instr", "lead", "coach")
role_colors = c("red", "green", "purple", "yellow", "blue")
gender2_colors = c("grey", "pink", "cyan")
gender3_colors = c("pink", "cyan")
dyad_colors = c("black", "red")
role_colors2 = c("red", "blue", "yellow", "purple", "green")
names(dyad_colors) <- c(FALSE, TRUE)

## Set up pallets...
# For bar graphs, etc.
role_fill <- scale_fill_manual(name = "role", values = role_colors)
sender_role_fill <- scale_fill_manual(name = "sender_role", values = role_colors)
receiver_role_fill <- scale_fill_manual(name = "receiver_role", values = role_colors)
dyad_fill <- scale_fill_manual(name = "dyad", values = dyad_colors)
# For line graphs, etc.
role_color <- scale_color_manual(name = "role", values = role_colors)
sender_role_color <- scale_color_manual(name = "sender_role", values = role_colors)
receiver_role_color <- scale_color_manual(name = "receiver_role", values = role_colors)
dyad_color <- scale_color_manual(name = "dyad", values = dyad_colors)