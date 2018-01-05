########################################################################################
## edge_trajectories.R
## plot strength of edges through time
########################################################################################

# run config file
source("config.R")

##########################
#### Helper Functions ####
##########################
plot_outedges_byperson <- function(edges, semester_a, night_a, role_a, scale_factor=3, point_size=0.5, alpha_a=0.5){
  # get subset of edges
  edges_subset <- edges[with(edges, semester==semester_a & night==night_a & sender_role==role_a),]
  # order them so lines plot correctly
  #edges_subset <- edges_subset[with(edges_subset, order(sender_final_id, !dyad, receiver_final_id, survnum)),]
  edges_subset <- edges_subset[with(edges_subset, order(sender_final_id, receiver_final_id, survnum)),]
  # setup plot
  grid_size <- as.integer(sqrt(length(unique(edges_subset$sender_final_id))))+1
  filen <- paste(paste(paste(output_dir, semester_a, "_outedges_byperson", sep="") , night_a, role_a, sep="_"), ".pdf", sep="")
  pdf(filen, width=grid_size*scale_factor, height=grid_size*scale_factor)
  # plot
  #dyad_edges = subset(edges_subset, dyad==TRUE)
  #non_dyad_edges = subset(edges_subset, dyad==FALSE)
  #pd <- position_dodge(0.1)
  p <- ggplot(edges_subset, aes(x=survnum, y=sn2, color=dyad, group=receiver_final_id))
  #p <- ggplot(edges_subset)
  #p <- p + geom_point(aes(x=survnum, y=sn2), data=non_dyad_edges, alpha=alpha_a, size=point_size, position=pd)
  #p <- p + geom_line(aes(x=survnum, y=sn2, group=pair_id), data=non_dyad_edges, alpha=alpha_a, position=pd)
  #p <- p + geom_point(aes(x=survnum, y=sn2), data=dyad_edges, alpha=alpha_a, size=point_size, position=pd)
  #p <- p + geom_line(data=dyad_edges, aes(x=survnum, y=sn2, group=pair_id), alpha=alpha_a, position=pd)
  p <- p + geom_point(alpha=alpha_a, size=point_size)
  p <- p + geom_line(alpha=alpha_a)
  p <- p + theme(legend.position="none", panel.grid.major.y=element_line(linetype=2, color="grey50")) + dyad_color + xlab("Suvey Number") + ylab("Relationship Strength")
  p <- p + scale_y_continuous(breaks=seq(0,10,2)) + scale_x_continuous(breaks=seq(0,5,1))
  p <- p + facet_wrap(~sender_final_id)
  p <- p + scale_alpha(guide="none")    
  print(p)
  # close the pdf
  dev.off()
}

plot_outedges_dyadonly <- function(edges, semester_a, night_a, role_a, scale_factor=3, point_size=1, alpha_a=0.5){
  # get subset of edges
  edges_subset <- edges[with(edges, semester==semester_a & night==night_a & sender_role==role_a & dyad==TRUE),]
  # order them so lines plot correctly
  edges_subset <- edges_subset[with(edges_subset, order(semester, night, sender_final_id, receiver_final_id, survnum)),]
  # setup plot
  grid_size <- as.integer(sqrt(length(unique(edges_subset$sender_final_id))))+1
  filen <- paste(paste(paste(output_dir, semester_a, "_outedges_dyadonly", sep="") , night_a, role_a, sep="_"), ".pdf", sep="")
  pdf(filen, width=grid_size*scale_factor, height=grid_size*scale_factor)
  # plot
  p <- ggplot(edges_subset, aes(x=survnum, y=sn2, color=dyad))
  p <- p + geom_point(alpha=alpha_a) + geom_line(aes(group=edges_subset$pair_id), alpha=alpha_a)
  p <- p + theme(legend.position="none", panel.grid.major.y=element_line(linetype=2, color="grey50")) + dyad_color + xlab("Suvey Number") + ylab("Relationship Strength")
  p <- p + scale_y_continuous(breaks=seq(0,10,2)) + scale_x_continuous(breaks=seq(0,5,1))
  p <- p + scale_alpha(guide="none")    
  print(p)
  # close the pdf
  dev.off()
}

dyad_trajectory <- function(edges, semester_a, night_a, scale_factor=3, point_size=1){
  # get subset of edges
  edges_subset <- edges[with(edges, semester==semester_a & night==night_a & dyad==TRUE),]
  # setup plot
  edges_subset$dyad_id <- substr(edges_subset$sender_final_id, 2, 999)
  # take only the columns we need
  edges_subset <- edges_subset[, c("dyad_id", "sender_role", "survnum", "sn2")]
  # reshape
  edges_subset = dcast(edges_subset, dyad_id + survnum ~ sender_role, value.var="sn2", fun.aggregate=sum)
  # order them so lines plot correctly
  edges_subset <- edges_subset[with(edges_subset, order(dyad_id, survnum)),]
 
  # plot each dyad separately 
  grid_size <- as.integer(sqrt(length(unique(edges_subset$dyad_id))))+1
  filen <- paste(paste(paste(output_dir, semester_a, "_dyad_trajectory", sep="") , night_a, sep="_"), ".pdf", sep="")
  pdf(filen, width=scale_factor*5, height=scale_factor*5)
  # plot
  colormap = palette(viridis(5, option="viridis"))
  p <- ggplot(edges_subset, aes(x=mentee, y=mentor, color=as.factor(survnum)))
  p <- p + geom_point() + geom_path(aes(group=edges_subset$dyad_id))
  p <- p + theme(legend.position="bottom", panel.grid.major=element_line(linetype=2, color="grey25"), panel.border=element_rect(colour="black", fill=NA, size=1)) 
  p <- p + labs(x="Mentee Strength", y="Mentor Strength", color="Survey Number")
  p <- p + scale_y_continuous(breaks=seq(0,10,5), limits=c(0,10)) + scale_x_continuous(breaks=seq(0,10,5), limits=c(0,10))
  p <- p + scale_color_manual(values=colormap)
  p <- p + facet_wrap(~edges_subset$dyad_id)
  print(p)
  dev.off()
 
  # plot average behavior instead
  averages <- aggregate(edges_subset[,c("survnum", "mentor", "mentee")], by=list(edges_subset$survnum), FUN="mean")
  filen <- paste(paste(paste(output_dir, semester_a, "_dyad_trajectory", sep="") , night_a, sep="_"), "_average.pdf", sep="")
  pdf(filen, width=scale_factor*3, height=scale_factor*3)
  # plot
  p <- ggplot(averages, aes(x=mentee, y=mentor, color=as.factor(survnum)))
  p <- p + geom_point() + geom_path(color=as.factor(averages$survnum))
  p <- p + theme(legend.position="bottom", panel.grid.major=element_line(linetype=2, color="grey25"), panel.border=element_rect(colour="black", fill=NA, size=1)) 
  p <- p + labs(x="Mentee Strength", y="Mentor Strength", color="Survey Number")
  p <- p + scale_y_continuous(breaks=seq(0,10,2), limits=c(0,10)) + scale_x_continuous(breaks=seq(0,10,2), limits=c(0,10))
  #p <- p + ylim(0, 10) + xlim(0, 10)
  p <- p + scale_color_manual(values=colormap)
  print(p)
  dev.off()
}


###############
#### Plots ####
###############
nights <- c("Mon", "Tue", "Wed", "Thu")
roles <- c("mentor", "mentee")

point_size <- 1
alpha <- .8
scale_factor <- 2

for(semester_a in semesters){
  for(night_a in nights){
    for(role_a in roles){
      plot_outedges_byperson(valid_edges, semester_a, night_a, role_a, scale_factor, point_size, alpha)
    }
  }
}
scale_factor <- 1
for(semester_a in semesters){
  for(night_a in nights){
    for(role_a in roles){
      plot_outedges_dyadonly(valid_edges, semester_a, night_a, role_a, scale_factor, point_size, alpha)
    }
  }
}
scale_factor <- 2
for(semester_a in semesters){
  for(night_a in nights){
    dyad_trajectory(valid_edges, semester_a, night_a, scale_factor, point_size)
  }
}