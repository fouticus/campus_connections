# load packages
library(gmodels)
library(stringr)
library(ggplot2)
library(gridExtra)
library(plyr)
theme_set(theme_classic())

# load data
source("load_and_clean2.R")

# gender breakdown
p_staff_gender <- ggplot(staff, aes(x=night)) + geom_bar(aes(fill=gender)) + facet_grid(semester ~ room) + theme(legend.position="bottom") + scale_fill_manual(values=c("grey", "pink", "cyan"))
p_staff_gender
p_mentee_gender <- ggplot(mentees, aes(x=night)) + geom_bar(aes(fill=gender)) + facet_grid(semester ~ room) + theme(legend.position="bottom") + scale_fill_manual(values=c("pink", "cyan"))
p_gender <- grid.arrange(p_mentee_gender, p_staff_gender, ncol=2)
p_gender

# role breakdown
p_staff_role <- ggplot(staff, aes(x=night)) + geom_bar(aes(fill=role)) + facet_grid(semester ~ room) + theme(legend.position="bottom") 
p_mentee_role <- ggplot(mentees, aes(x=night)) + geom_bar(aes(fill=role)) + facet_grid(semester ~ room) + theme(legend.position="bottom")
p_role <- grid.arrange(p_mentee_role, p_staff_role, ncol=2)
p_role


# what is the node degree of each graph?