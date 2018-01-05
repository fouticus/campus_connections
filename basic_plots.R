########################################################################################
## basic_plots.R
## performs some initial visualization of participants
########################################################################################

# run config file
source("config.R")


# gender breakdown
filename = paste(output_dir, "participant_gender.pdf", sep="")
pdf(filename, width=10, height=6)
p_staff_gender <- ggplot(staff, aes(x=night)) + geom_bar(aes(fill=gender), color="black") + facet_grid(semester ~ room) + theme(legend.position="bottom") + scale_fill_manual(values=c("grey", "pink", "cyan")) + ggtitle("Mentors")
p_mentee_gender <- ggplot(mentees, aes(x=night)) + geom_bar(aes(fill=gender), color="black") + facet_grid(semester ~ room) + theme(legend.position="bottom") + scale_fill_manual(values=c("pink", "cyan")) + ggtitle("Mentees")
p_gender <- grid.arrange(p_mentee_gender, p_staff_gender, ncol=2)
p_gender
dev.off()

# role breakdown
filename = paste(output_dir, "participant_roles.pdf", sep="")
pdf(filename, width=10, height=6)
p_staff_role <- ggplot(staff, aes(x=night)) + geom_bar(aes(fill=role), color="black") + facet_grid(semester ~ room) + theme(legend.position="bottom") + ggtitle("Mentors")
p_staff_role <- p_staff_role + role_fill
p_mentee_role <- ggplot(mentees, aes(x=night)) + geom_bar(aes(fill=role), color="black") + facet_grid(semester ~ room) + theme(legend.position="bottom") + ggtitle("Mentees")
p_mentee_role <- p_mentee_role + role_fill
p_role <- grid.arrange(p_mentee_role, p_staff_role, ncol=2)
p_role
dev.off()

#################
#### Cleanup ####
#################
rm(p_gender)
rm(p_mentee_gender)
rm(p_mentee_role)
rm(p_role)
rm(p_staff_gender)
rm(p_staff_role)
