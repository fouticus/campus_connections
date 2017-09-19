source("config.R")
source("functions.R")

# clean up edge lists
valid_edges <- edges[with(edges, sender_missing==0 & receiver_missing==0 & sn1==1 & !is.na(sn2) & sender_final_id!=receiver_final_id), ]
roles <- participants[,c("final_id", "role", "night")]
valid_edges <- merge(valid_edges, roles, by.x=c("sender_final_id", "night"), by.y=c("final_id", "night"))
colnames(valid_edges)[length(colnames(valid_edges))] <- "sender_role"
valid_edges <- merge(valid_edges, roles, by.x=c("receiver_final_id", "night"), by.y=c("final_id", "night"))
colnames(valid_edges)[length(colnames(valid_edges))] <- "receiver_role"
valid_edges$pair_id <- paste(valid_edges$sender_final_id, valid_edges$receiver_final_id, sep="->")
valid_edges$dyad <- substr(valid_edges$sender_final_id, 2, 999) == substr(valid_edges$receiver_final_id, 2, 999)

# ordering happens in the plot functions now
#valid_edges <- valid_edges[with(valid_edges, order(semester, night, sender_final_id, receiver_final_id, survnum)),]


semesters <- c("Fa15")
nights <- c("Mon", "Tue", "Wed", "Thu")
#nights <- c("Thu")
roles <- c("mentor", "mentee")

point_size <- 1
scale_factor <- 3

for(semester_a in semesters){
  for(night_a in nights){
    for(role_a in roles){
      #plot_outedges_byperson(valid_edges, semester_a, night_a, role_a, scale_factor, point_size)
    }
  }
}
scale_factor <- 1
for(semester_a in semesters){
  for(night_a in nights){
    for(role_a in roles){
      #plot_outedges_dyadonly(valid_edges, semester_a, night_a, role_a, scale_factor, point_size)
    }
  }
}
scale_factor <- 2
for(semester_a in semesters){
  for(night_a in nights){
    dyad_trajectory(valid_edges, semester_a, night_a, scale_factor, point_size)
  }
}