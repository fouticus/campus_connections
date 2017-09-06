#               mentor, coach, instr, lead, mentee
role_colors = c("red", "green", "purple", "yellow", "blue")
#roles = c("mentor", "mentee", "instr", "lead", "coach")
gender2_colors = c("grey", "pink", "cyan")
gender3_colors = c("pink", "cyan")


library(RColorBrewer)
#role_pallete <- brewer.pal(5,"Set1")
role_pallete <- role_colors
names(role_pallete) <- levels(participants$role)
role_fill <- scale_fill_manual(name = "role", values = role_pallete)
