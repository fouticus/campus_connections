source("config.R")
source("functions.R")

nights <- c("Mon", "Tue", "Wed", "Thu")
#nights <- c("Thu")
roles <- c("mentor", "mentee")

point_size <- 1
alpha=0.3
scale_factor <- 3

for(semester_a in semesters){
  for(night_a in nights){
    for(role_a in roles){
      #plot_outedges_byperson(valid_edges, semester_a, night_a, role_a, scale_factor, point_size, alpha)
    }
  }
}
scale_factor <- 1
for(semester_a in semesters){
  for(night_a in nights){
    for(role_a in roles){
      #plot_outedges_dyadonly(valid_edges, semester_a, night_a, role_a, scale_factor, point_size, alpha)
    }
  }
}
scale_factor <- 2
for(semester_a in semesters){
  for(night_a in nights){
    dyad_trajectory(valid_edges, semester_a, night_a, scale_factor, point_size)
  }
}